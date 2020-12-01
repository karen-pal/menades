module ToFile
    ( exportPictureToPNG
    ) where

import Codec.Picture.Png (writePng)
import qualified Graphics.Gloss.Rendering as GlossR

import File

-- | Save a gloss Picture as PNG
exportPictureToPNG :: Size -- ^ width, height in pixels 
                   -> GlossR.Color -- ^ background color
                   -> FilePath -> GlossR.Picture -> IO ()
exportPictureToPNG  = exportPictureToFormat writePng
