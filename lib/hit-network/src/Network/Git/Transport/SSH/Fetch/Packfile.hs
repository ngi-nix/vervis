{- This file is part of hit-network.
 -
 - Written in 2016 by fr33domlover <fr33domlover@riseup.net>.
 -
 - ♡ Copying is an act of love. Please copy, reuse and share.
 -
 - The author(s) have dedicated all copyright and related and neighboring
 - rights to this software to the public domain worldwide. This software is
 - distributed without any warranty.
 -
 - You should have received a copy of the CC0 Public Domain Dedication along
 - with this software. If not, see
 - <http://creativecommons.org/publicdomain/zero/1.0/>.
 -}

module Network.Git.Fetch.Packfile
    ( minimalSet
    , buildPack
    )
where

data Hint = Missing | Shallow | Healthy

-- | Flatten a forest, dropping all top-level nodes.
flattenDrop :: [Tree a] -> [a]
flattenDrop = concatMap flatten . concatMap subForest

-- | After negotiation is over, we know which refs the client wants and we have
-- a list of commits it has locally. Now we need to determine which commits to
-- send.
--
-- The most naive solution, which wouldn't even require negotiation, is to
-- simply send the /entire/ repository every single time. But that's a waste of
-- time and bandwidth of course. In many cases the clients just needs to get
-- the latest commits made in the last hours. Therefore we do negotiate.
--
-- A slightly less naive solution is for the client to send us a full list of
-- the commits it has. Then we take the set of commits we have, subtract what
-- the client says it has, and we need to send just this difference.
--
-- But even that can be made better. When the client tells us it has a certain
-- commit C, and it didn't list C as shallow, we know the client also has the
-- /parent/ commits of C. And for each such parent, if it wasn't specified as
-- shallow either, we also know the client has its parents too, and so on. In
-- other words, if the client has a non-shallow commit C and a parent of it P,
-- it's enough to tell us it has C, and P doesn't have to be mentioned. This
-- allows the client to send us a much shorter list of commits, and we on the
-- server figure out the rest.
--
-- This can make a huge difference! Suppose we have a repo with a single branch
-- and 1000 commits. The client has 999 of them and wants to fetch the last
-- one. Instead of sending us the IDs of the 999 commits it has, it can send
-- just the ID of the latest it has, the 999th one, and we figure out it has
-- the other 998 commits locally. Therefore less data is sent over the network.
--
-- Note that when the client sends us /have/ lines, the commits mentioned won't
-- necessarily be ancestors of the refs it /wants/. For example, it may
-- mention the tips of all branches it has. Therefore, we must run the
-- deduction using a graph of /all/ commits, not a subgraph containing just the
-- ancestors of the /wants/.
--
-- We can determine the minimal set of commits to send like this:
--
-- (1) In the graph of all commits, label each commit with one of three labels:
--     Missing, Shallow or Healthy. If a commit was mentioned by the client as
--     /have/ but not as /shallow/, label it with Healthy. If it was mentioned
--     as /shallow/, label it with Shallow. Otherwise, label it with Missing.
-- (2) Run a custom DFS in the graph, starting from all the Healthy nodes. This
--     DFS is like regular DFS, except we visit a commit's parent only if it's
--     labeled with Missing. Otherwise we skip the parent. Return a tree
--     representing the order of traversal. In this tree, the top level
--     contains all the Healthy nodes, and the rest is Missing nodes.
-- (3) Take all the non-toplevel nodes in the tree, and mark them all as
--     Healthy in the graph.
-- (4) Make a subgraph containing just the refs the client /wants/ and their
--     ancestor commits. If there's a depth limit, also drop the commits deeper
--     than the limit.
-- (5) Make a list of all the Missing nodes in the subgraph. These are the
--     commits we'll have to send the client.
minimalCommitSet
    :: CommitGraph g -- graph of ALL commits, i.e. made of /all/ branches
    -> HashMap ObjId (Node, Commit) -- ALL refs, i.e. made of /all/ branches
    -> Maybe (Depth, HashMap Node Depth)
    -> [Node]
    -> [ObjId]
    -> [ObjId]
    -> [(Node, (ObjId, Commit))]
minimalCommitSet graph objmap mdepths wants shallows haves =
    let -- (1) Initial labels for commits
        haveSet = S.fromList haves
        shallowSet = S.fromList shallows
        healthySet = haveSet `S.difference` shallowSet
        hint r =
            if r `S.member` healthyList
                then Healthy
                else if r `S.member` shallowSet
                    then Shallow
                    else Missing
        graph' = nmap (\ (r, c) -> (r, c, hint r)) graph
        -- (2) Traverse Missing ancestors of Healthy commits
        isMissing n =
            case getLabel graph' n of
                (_, _, Missing) -> True
                _               -> False
        objmapH = objmap `M.intersection` toMap healthySet
        healthyNodes = map (fst . snd) $ M.fromList objmapH
        nexts n = filter isMissing $ suc' n
        healthyTree = xdffWith nexts node' healthyNodes graph'
        -- (3) Mark traversed Missing nodes as Healthy
        newHealthy = S.fromList $ flattenDrop healthyTree
        mark n l@(r, c, Missing) =
            if n `S.member` newHealthy
                then (r, c, Healthy)
                else l
        mark n l = l
        graph'' = gmap (\ (p, n, l, s) -> (p, n, mark n l, s)) graph'
        -- (4) Pick the commits the clients wants and not-too-deep ancestors
        included dmap thresh n =
            case M.lookup n dmap of
                Nothing -> True
                Just d  -> d <= thresh
        next = case mdepths of
            Nothing             -> suc'
            Just (thresh, dmap) -> filter (included dmap thresh) . suc'
        wantsWithInclDeps = xdfsWith next node' wants graph''
        subGraph = subgraph wantsWithInclDeps graph''
    -- (5) List the Missing nodes in the subgraph
    in  [(n, (r, c)) | (n, (r, c, Missing)) <- labNodes subGraph]

-- It seems, judging by the git C code and the git docs, that without sideband
-- modes, we just need to stream the packfile raw over the socket. How do we
-- efficiently lazily do so, in parallel to that file being built by another
-- function which feeds it to us?
--
-- So I went to the @bytestring@ and @pipes-binary@ packages to check how lazy
-- bytestrings are streamed. Both in the Pipes case and in the bytestring IO
-- case, the method is to 'foldrChunks' and send each chunk separately.
-- Therefore, all I need to do here is:
--
-- (1) Build the packfile as a lazy bytestring using Data.Binary.Put
-- (2) @send@ it just like I did all the other Puts so far
--
-- Now there are at least 2 aspects to handle here:
--
-- (1) The logical construction of the pack components
-- (2) Encoding them according to the pack format
--
-- The first part involves a process of converting objects to deltas. We sort
-- the objects according to a certain order, and then fold over them with a
-- sliding window, and inside the window we try to diff between every pair of
-- objects. When the diff is small, one object is replaced by the diff, so that
-- the pack is smaller, and the original object is reconstructed on the other
-- side using the diff and the object on which the diff was based. These diffed
-- objects are called deltas. The idea of the sorting order is to try to put
-- objects that are likely to be similar close to each other in the order, so
-- that they end up together in the sliding window.
--
-- For the second part, which I'll try to handle first, there is a precise
-- definition of the pack format. @hit@ seems to have a reader for it, which
-- may be great for @receive-pack@ (although I may decide to use
-- Data.Binary.Get anyway and my own code), but there's no API for creating a
-- pack. So I'm implementing it here.
--
-- By the way, @hit@ implements its own @FileReader@ which is basically like
-- the stream decoding we do here (i.e. 'decodeFileOrFail' and my 'receive'),
-- but also supports decompression because it seems git data may be stored
-- zipped. Instead of that I could use @binary@'s tools, and just find a good
-- way to implement compression, if it's needed at all for @receive-pack@.

-- The next question is exactly which objects to include. The deltas and the
-- packing order are optimizations I can hopefully initially skip. Given the
-- list of 'Missing' commits, which objects do I pack, and exactly what do I
-- send to the client? Do I also need the pack index? Let's dive further into
-- docs and code.
--
-- In @upload-pack.c@ we have this sequence:
--
-- > receive_needs()
-- > get_common_commits()
-- > create_pack_file()
--
-- I assume receive_needs() is the part where the UploadRequest is received
-- (wants, shallows, depth limit). Skipping
--
-- I assume get_common_commits() is where the UploadHaves is received, and the
-- server figures out what to send. Skipping
--
-- Let's go to create_pack_file(). This function starts by preparing to fork a
-- separate git command, @git pack-objects@. I'll try to summarize the
-- important parts. I'm also going to assume no capabilities are supported,
-- such as thin-pack. So I'll skip things related to them.
--
-- * Make list of args
--   * If shallow objects exist, "--shallow-file"
--   * "pack-objects"
--   * "--revs"
-- * Run the "git" command with those args
-- * Write to the command's stdin via a pipe
--   * commit.c holds a local array of commits grafts (what's a graft?), each
--     of which is a SHA1 and a list of parents. If there are shallow objects,
--     we iterate over all the graft list (which I suppose was filled earlier)
--     and for each one, we check if its list of parents is empty. If yes, we
--     write the line "--shallow %s\n" to the pipe, where %s is the hex string
--     of the SHA1 of the graft. I think it basically means we tell @git
--     upload-pack@ about all the shallow commits we want to pack.
--   * We write to the pipe, 1 per line, the hex strings of the SHA1s of all
--     the "want" refs (i.e. what the client requested). I assume this is what
--     the want_obj array contains (FIXME verify this if needed).
--   * We write a "--not\n" line to the pipe
--   * We write, one per line, hexes of SHA1s from the have_obj array
--   * We write, one per line, hexes of SHA1s from the extra_edge_obj array
--   * We write a blank line to the pipe "\n"
-- * Close the pipe
-- * Read from the command's stdout via a pipe. Using a loop, incrementally
--   read the packfile and send it over the socket to the client side.
--
-- Now we have 2 directions to proceed:
--
-- (1) Find out exactly what want_obj, have_obj and extra_edge_obj lists
--     contain when create_pack_file() is called
-- (2) Figure out how @git pack-objects@ works, specifically exactly which
--     objects it puts in the pack, whether it returns just a packfile or also
--     a pack index and so on.
--
-- * want_obj: commits the client /wants/ and parents of commits the client has
--             shallow, but we report them as /unshallow/ so will send their
--             parents
-- * have_obj: objects the client sent as /have/ but without oids which are
--             parents of other /have/ oids
-- * extra_edge_obj: commits the client reported as shallow
--
-- The @git pack-objects@ command is implemented by function @cmd_pack_objects@
-- in file @builtin/pack-objects.c@. Let's see how it works. First, what do the
-- options specified mean?
--
-- * --shallow-file: no idea, it seems to mean "don't use a shallow file" but
--                   in the fetch-pack command, and not used elsewhere. Anyway
--                   we'll see.
-- * --revs : read revision arguments from standard input
--
-- Now here's roughly how the function works:
--
-- * Parse arguments and set all the options and possible mode flags
--
-- * prepare_packed_git()
--   From @sha1_file.c@, seems to read packs from the git repo. Perhaps
--   somewhat like @hit@'s 'openRepository'
--
-- * get_object_list()
--   Reads the --shallow and --not and the SHA1s passed by upload-pack. Does
--   some commit graph traversal, not sure why exactly. I'll assume for now it
--   basically stores these lists for later use, maybe also processes them
--   already to decide which objects to pack.
--   * for each stdin SHA1: handle_revision_arg(line, &revs, flags, REVARG_CANNOT_BE_FILENAME)
--   * prepare_revision_walk(&revs)
--   * mark_edges_uninteresting(&revs, show_edge)
--   * traverse_commit_list(&revs, show_commit, show_object, NULL)
--
-- * cleanup_preferred_base()
--   Frees data in some data structure. Some sort of cache, not sure what it
--   does.
--
-- * prepare_pack(window, depth)
--   After some mode variable assignments, there's a loop which goes over an
--   object array named @to_pack@. Looks like this function goes over the
--   objects, sorts them ("pack heuristics") and finds deltas using a sliding
--   window.
--
-- * write_pack_file()
--   I see two interesting parts:
--   * compute_write_order()
--     Make an object array for write order. I don't know why this order
--     specifically. A brief look in the git docs didn't reveal this info, but
--     it may be there if I read the pack heuristics file more thoroughly.
--   * a big do-while loop
--     It loops over objects until there are no remaining objects to pack.
--     Basically, when writing to stdout and without reuse or deltas - which is
--     our case - it simply writes the pack header, and then goes over the
--     write_order array and writes the objects one by one, using the write_one
--     function. Finally, it writes the hash of all the content.
--     * write_one(f, e, &offset)
--       * buf = read_sha1_file(entry->idx.sha1, &type, &size)
--         * read_sha1_file_extended
--           * repl = lookup_replace_object_extended(sha1, flag)
--           * read_object(repl, type, size)
--       * datalen = do_compress(&buf, size)
--         Uses zlib to compress the data
--       * hdrlen = encode_in_pack_object_header(type, size, header);
--       * sha1write(f, header, hdrlen)
--       * sha1write(f, buf, datalen)

-- | Recursively follow the treeish of the given commit and add the objects IDs
-- to the set. When the root tree is found, check if it's already in the set.
-- If yes, return 'Nothing'. Otherwise insert it, and return 'Just' the root
-- tree.
iterateCommitTreeish :: Git -> ObjIdSet -> Commit -> IO (ObjIdSet, Maybe Tree)

-- | Recursively follow the treeish of the given tag and add the objects IDs
-- to the set. When the root tree is found, check if it's already in the set.
-- If yes, return 'Nothing'. Otherwise insert it, and return 'Just' the root
-- tree.
iterateTagTreeish :: Git -> ObjIdSet -> Tag -> IO (ObjIdSet, Maybe Tree)

-- | Take a minimal list of commits we must send, and build a set of object IDs
-- of these commits and all the trees and blobs they refer to recursively.
--
-- I'm not exactly sure yet this is the right way to build a pack. Does a
-- commit's treeish always refer (after delta resolution) to a Tree? What if it
-- refers to a commit, specifically one we aren't going to send? And should we
-- really just send all the trees and blobs we find?
--
-- I'm going to implement it as-is and then test @git pull@ and see what
-- happens. The steps are roughly these:
--
-- (1) Make a set of the commit IDs we got
-- (2) Resolve the treeish of each commit, and push the IDs we find along the
--     way to the set. So now we have the updated set and the root trees of all
--     the commits.
-- (3) Scan the all the trees and push the IDs of all the trees and blobs we
--     find to the set
--
-- TODO fix the code, wherever it is, to work according to the algo above
