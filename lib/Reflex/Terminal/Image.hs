module Reflex.Terminal.Image (Axis (..), Image (..), Renderable (..)) where

import Control.Arrow
import Control.Monad.RWS (MonadReader (..))
import Data.Char.WCWidth
import Data.Text qualified as T
import Reflex.Terminal.Interval
import Reflex.Terminal.Prelude
import Reflex.Terminal.Rect hiding (height, width)
import System.Console.ANSI
import System.IO

-- data Span = Span {text :: T.Text} deriving (Show)

data Axis = Row | Column deriving (Show)

data Image
  = Span {text :: T.Text}
  | -- Line {spans :: [Span]}
    Container
      { axis :: Axis,
        children :: [Image]
      }
  deriving (Show)

instance HasField "height" Image Int where
  getField Span {} = 1
  getField container@Container {}
    | Row <- container.axis = foldr1 max ((.height) <$> container.children)
    | Column <- container.axis = sum ((.height) <$> container.children)

-- instance HasField "height" Span Int where
--   getField span = 1

-- instance HasField "width" Span Int where
--   getField span = T.foldl' (\l c -> wcwidth c + l) 0 span.text

instance HasField "width" Image Int where
  getField span@Span {} = T.foldl' (\l c -> wcwidth c + l) 0 span.text
  getField container@Container {}
    | Row <- container.axis = sum ((.width) <$> container.children)
    | Column <- container.axis = foldr1 max ((.width) <$> container.children)

class Renderable a where
  renderWithClipping :: (MonadRender m) => Rect Int -> a -> m ()

-- instance Renderable Span where
--   -- TODO: add clipping
--   renderWithClipping _ span = do
--     cursor <- ask
--     liftIO $ do
--       hPrint stderr ("?", span, cursor)
--       uncurry setCursorPosition cursor
--       setSGR (Reset : [])
--       putStr $ T.unpack span.text

instance Renderable Image where
  renderWithClipping clip span@Span {}
    | 0 <- clip.top = do
        cursor <- ask
        -- todo: add horizontal clipping
        liftIO $ do
          hPrint stderr ("?", span, cursor)
          uncurry setCursorPosition cursor
          setSGR (Reset : [])
          putStr $ T.unpack span.text
    | otherwise = pure ()
  renderWithClipping clip container@Container {axis} = do
    let sizeAlongAxis = case axis of
          Row -> (.width)
          Column -> (.height)
        intervalL :: Lens' (Rect Int) (Interval Int)
        intervalL = case axis of
          Row -> xInterval
          Column -> yInterval

    void $ (`execStateT` 0) do
      runExceptT $ do
        for_ (container.children :: [Image]) \child -> do
          offset :: Int <- lift get
          let childInterval = Interval (offset, offset + sizeAlongAxis child)

          liftIO $ hPrint stderr ("!", offset, childInterval)
          let withOrigin :: (MonadReader (Int, Int) m) => m a -> m a
              withOrigin m =
                do
                  case axis of
                    Row ->
                      local (second (+ (offset - clip ^. intervalL . _Wrapped . _1))) m
                    Column ->
                      local (first (+ (offset - clip ^. intervalL . _Wrapped . _1))) m

          case intersection childInterval (clip ^. intervalL) of
            Before -> pure ()
            OverlapsLeft overlap ->
              withOrigin $
                renderWithClipping
                  (clip & intervalL .~ Interval (overlap ^. _Wrapped . _1 - offset, overlap.size))
                  child
            Contains -> undefined
            ContainedBy ->
              withOrigin $
                renderWithClipping
                  (clip & intervalL .~ Interval (0, sizeAlongAxis child))
                  child
            OverlapsRight overlap ->
              withOrigin $
                renderWithClipping
                  (clip & intervalL .~ Interval (0, overlap.size))
                  child
            After -> throwError ()
          lift $ modify (+ sizeAlongAxis child)

spanM :: forall m a. (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
spanM predicate = go []
  where
    go :: [a] -> [a] -> m ([a], [a])
    go acc [] = pure (reverse acc, [])
    go acc rest@(x : xs) = do
      matches <- predicate x
      if matches
        then go (x : acc) xs
        else pure (reverse acc, rest)

type MonadRender m = (MonadReader (Int, Int) m, MonadIO m)
