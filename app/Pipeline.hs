{-# LANGUAGE DeriveFunctor #-}

module Pipeline where

-- Pipeline type that can chain operations with error handling
newtype Pipeline a b = Pipeline {runPipeline :: a -> IO (Either String b)}
  deriving (Functor)

instance Applicative (Pipeline a) where
  pure x = Pipeline $ \_ -> return (Right x)
  Pipeline f <*> Pipeline x = Pipeline $ \a -> do
    fResult <- f a
    case fResult of
      Left err -> return (Left err)
      Right func -> do
        xResult <- x a
        case xResult of
          Left err -> return (Left err)
          Right val -> return (Right (func val))

instance Monad (Pipeline a) where
  Pipeline x >>= f = Pipeline $ \a -> do
    result <- x a
    case result of
      Left err -> return (Left err)
      Right b -> runPipeline (f b) a

-- Compose pipelines
(>>>) :: Pipeline a b -> Pipeline b c -> Pipeline a c
Pipeline f >>> Pipeline g = Pipeline $ \a -> do
  result1 <- f a
  case result1 of
    Left err -> return (Left err)
    Right b -> g b

-- Lift a function into a pipeline
liftPipeline :: (a -> IO (Either String b)) -> Pipeline a b
liftPipeline = Pipeline

-- Lift a pure function into a pipeline
purePipeline :: (a -> b) -> Pipeline a b
purePipeline f = Pipeline $ \a -> return (Right (f a))

-- Run a pipeline with error handling
runPipelineWithError :: Pipeline a b -> a -> IO b
runPipelineWithError pipeline input = do
  result <- runPipeline pipeline input
  case result of
    Left err -> error err
    Right output -> return output