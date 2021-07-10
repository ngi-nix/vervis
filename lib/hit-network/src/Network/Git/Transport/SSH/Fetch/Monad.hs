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

module Network.Git.Fetch.Monad
    (
    )
where

data GitProtoFail = GitProtoFail
    { gpfRemainder :: B.ByteString
    , gpfConsumed  :: Int64
    , gpfReason    :: String
    , gpfSituation :: String
    }
    deriving Show

type FetchT m =
    TimeoutT
        ( ExceptT GitProtoFail
            ( StateT (Maybe B.ByteString)
                ( StreamT
                    ( GitT m
                    )
                )
            )
        )

liftM      = lift . lift . lift . lift . lift

liftM'     = lift . lift . lift . lift

liftGit    = lift . lift . lift . lift

liftStream = lift . lift . lift

liftBuffer = lift . lift

liftFail   = lift

send :: MonadLogger m => Put -> Fetch m ()
send put = do
    writeBytes <- liftStream $ asks fst
    let bytes = runPut put
    logDebugN $ "Sending " <> decodeLatin1 bytes
    liftM $ writeBytes bytes

receive :: MonadLogger m => String -> Get a -> Fetch m a
receive desc decode = go $ runGetIncremental decode
    where
    go (Fail remains consumed err) = do
        let gpf = GitProtoFail
                { gpfRemainder = remains
                , gpfConsumed  = consumed
                , gpfReason    = err
                , gpfSituation = desc
                }
        logErrorN $ T.pack $ show gpf
        liftFail $ throwE gpf
    go (Done unused consumed result) = do
        logDebugN $ sformat
            ("Decoded " % string % ", consumed " % int % " bytes")
            desc consumed
        mbuffer <- liftBuffer get
        case mbuffer of
            Nothing -> unless (B.null unused) $ liftBuffer $ put $ Just unused
            Just b  -> do
                logWarningN $ sformat
                    ( "Done decoding " % string % " with nonempty buffer: \""
                    % stext % "\""
                    )
                    desc (TE.decodeLatin1 b)
                unless (B.null unused) $ liftBuffer $ put $ Just $ b <> unused
        return result
    go (Partial k) = do
        mbuffer <- liftBuffer get
        s <- case mbuffer of
            Nothing -> do
                readBytes <- liftStream $ asks snd
                bytes <- withTimeoutThrow $ liftM' readBytes
                logDebugN $ "Received " <> TE.decodeLatin1 bytes
                return bytes
            Just b -> do
                liftBuffer $ put Nothing
                logDebugN $ "Reading buffer: " <> TE.decodeLatin1 b
                return b
        if B.null s
            then go $ k Nothing
            else go $ k $ Just s
