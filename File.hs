{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables #-}
module File
    ( Size
    , Animation
    , withGlossState
    , withImage
    , withImages
    , exportPictureToFormat
    , exportPicturesToFormat
    ) where

import Codec.Picture.Types (Image(..), Pixel, PixelRGBA8, PixelRGB8, componentCount)
import Control.Exception (bracket, bracket_)
import Control.Monad (forM_, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy(..))
import Data.Vector.Storable (unsafeFromForeignPtr0)
import qualified Graphics.Gloss.Rendering as Gloss
import Graphics.GL -- as GL*
import qualified Graphics.UI.GLFW as GLFW
import Foreign (newForeignPtr_)
import Foreign.Marshal.Array (allocaArray)
import Text.Printf (printf)
import GHC.Int
#ifdef linux_HOST_OS
import qualified Graphics.UI.GLUT as GLUT
#endif
import Prelude hiding (concat)

type Size = (Int, Int)
type Animation = Float -> Gloss.Picture

-- | Save a gloss Picture to a file.
exportPictureToFormat :: (FilePath -> Image PixelRGBA8 -> IO ()) -- ^ function that saves an intermediate representation to a format. Written with writeXY from Codec.Picture in mind
                      -> Size                -- ^ (width, heigth) in pixels - as in Gloss.Display
                      -> Gloss.Color         -- ^ Background color
                      -> FilePath -> Gloss.Picture -> IO ()
exportPictureToFormat savefunc size bgc f p = do
    withGlossState size $ \s -> do
      withImage size bgc s p $ \img -> do
        savefunc f img

-- | Acquire the Gloss.State required by the withImage* functions.
-- This allows the same OpenGL surface (of the given size) to be reused several times, which in turn makes Gloss bitmaps faster to render because their textures are kept in video memory.
withGlossState :: Size -> (Gloss.State -> IO a) -> IO a
withGlossState size body = do
#ifdef linux_HOST_OS
    _ <- GLUT.exit                     -- otherwise 'illegal reinitialization'
    (_,_) <- GLUT.getArgsAndInitialize -- needed for text  https://github.com/elisehuard/game-in-haskell/pull/3
#endif
    s <- Gloss.initState
    withGLFW $ do
      GLFW.windowHint (GLFW.WindowHint'Visible False)
      withWindow size "Image context" Nothing Nothing $ \window -> do
        GLFW.makeContextCurrent (Just window)
        body s

-- | A bracket API for GLFW.setErrorCallback which makes it easier to throw an exception upon failure.
withThrowGLFWError :: ((forall r. IO r) -> IO a) -> IO a
withThrowGLFWError body = do
    errorMessageRef <- newIORef "GLFW failed without an error message"

    let throwGLFWError :: IO r
        throwGLFWError = do
          errorMessage <- readIORef errorMessageRef
          error errorMessage

        errorCallback :: GLFW.ErrorCallback
        errorCallback _ errorMessage = do
          writeIORef errorMessageRef errorMessage

        acquire :: IO ()
        acquire = GLFW.setErrorCallback (Just errorCallback)

        release :: IO ()
        release = GLFW.setErrorCallback Nothing

    bracket_ acquire release $ do
      body throwGLFWError

-- | A bracket API for GLFW.init which throws an exception on failure.
withGLFW :: IO a -> IO a
withGLFW body = do
    withThrowGLFWError $ \throwGLFWError -> do
      bracket acquire release $ \glfwIsInitialized -> do
        if glfwIsInitialized then body else throwGLFWError
  where
    acquire :: IO Bool
    acquire = GLFW.init

    release :: Bool -> IO ()
    release glfwIsInitialized = when glfwIsInitialized GLFW.terminate

-- A bracket API for GLFW.createWindow which throws an exception on failure.
-- Must be called within withGLFW.
withWindow :: Size -> String -> Maybe GLFW.Monitor -> Maybe GLFW.Window
           -> (GLFW.Window -> IO a) -> IO a
withWindow (width, height) title monitor sharedContext body = do
    withThrowGLFWError $ \throwGLFWError -> do
      bracket acquire release $ \maybeWindow -> case maybeWindow of
        Just window -> body window
        Nothing -> throwGLFWError
  where
    acquire :: IO (Maybe GLFW.Window)
    acquire = GLFW.createWindow width height title monitor sharedContext

    release :: Maybe GLFW.Window -> IO ()
    release = mapM_ GLFW.destroyWindow




-- | Save a series of gloss Picture to files of spcified format.
exportPicturesToFormat :: (FilePath -> Image PixelRGBA8 -> IO ()) -- ^ function that saves an intermediate representation to a format. Written with writeXY from Codec.Picture in mind
                       -> Size                -- ^ (width, height) in pixels - as in Gloss.Display
                       -> Gloss.Color         -- ^ background color
                       -> FilePath            -- ^ must contain "%d", will be replaced by frame number
                       -> Animation           -- ^ function that maps from point in time to Picture. analog to Gloss.Animation
                       -> [Float]             -- ^ list of points in time at which to evaluate the animation
                       -> IO ()
exportPicturesToFormat savefunc size bgc f anim ts = do
    withGlossState size $ \s -> do
      forM_ (zip [1..] ts) $ \(n, t) -> do
        let filename = printf f (n :: Int)
        let picture = anim t
        withImage size bgc s picture $ \img -> do
          savefunc filename img


class Pixel pixel => OpenGLPixel pixel where
  openGLPixelFormat :: proxy pixel -> GLenum
  openGLPixelType   :: proxy pixel -> GLenum

instance OpenGLPixel PixelRGBA8 where
  openGLPixelFormat _ = GL_RGBA
  openGLPixelType   _ = GL_UNSIGNED_BYTE

instance OpenGLPixel PixelRGB8 where
  openGLPixelFormat _ = GL_RGB
  openGLPixelType   _ = GL_UNSIGNED_BYTE

-- | convert a gloss 'Picture' into an 'Image'.
withImage :: forall pixel a. OpenGLPixel pixel
          => Size                -- ^ (width, height) in pixels - as in Gloss.Display
          -> Gloss.Color         -- ^ Background color
          -> Gloss.State         -- ^ Obtained via 'withGlossState'
          -> Gloss.Picture
          -> (Image pixel -> IO a) -> IO a
withImage (windowWidth, windowHeight) bgc s p body = do
    let bytesPerPixel :: Int
        bytesPerPixel = componentCount (undefined :: pixel)

        pixelFormat :: GLenum
        pixelFormat = openGLPixelFormat (Proxy :: Proxy pixel)

        pixelType :: GLenum
        pixelType = openGLPixelType (Proxy :: Proxy pixel)

    --- the drawn image is flipped ([rowN,...,row1]) so we need to draw it upside down
    --- I guess this is because the origin is specified as topleft and bottomleft by different functions
    let flippedPicture :: Gloss.Picture
        flippedPicture = Gloss.Scale 1 (-1) p
    drawReadBuffer (windowWidth, windowHeight) bgc s flippedPicture
    allocaArray (windowWidth * windowHeight * bytesPerPixel) $ \imageData -> do
      let wW = fromIntegral windowWidth  :: GHC.Int.Int32
      let wH = fromIntegral windowHeight :: GHC.Int.Int32
      glReadPixels 0 0 wW wH pixelFormat pixelType imageData 
      foreignPtr <- newForeignPtr_ imageData
      let vector = unsafeFromForeignPtr0 foreignPtr (windowWidth * windowHeight * bytesPerPixel)
      let image :: Image pixel
          image = Image windowWidth windowHeight vector
      
      body image

withImages :: OpenGLPixel pixel
           => Size                -- ^ (width, height) in pixels - as in Gloss.Display
           -> Gloss.Color         -- ^ Background color
           -> Gloss.State         -- ^ Obtained via 'withGlossState'
           -> [Gloss.Picture]
           -> ([Image pixel] -> IO a) -> IO a
withImages _ _ _ [] body = body []
withImages size bgc s (p:ps) body = do
  withImage size bgc s p $ \image -> do
    withImages size bgc s ps $ \images -> do
      body (image:images)

drawReadBuffer :: Size
          -> Gloss.Color -- ^ Background color
          -> Gloss.State -> Gloss.Picture -> IO ()
drawReadBuffer size bg s p = do
    glDrawBuffer GL_BACK
    Gloss.withClearBuffer bg $ Gloss.withModelview size $ do
                                                           glColor3f 0 0 0
                                                           Gloss.renderPicture s 1 p
    glReadBuffer GL_BACK 
