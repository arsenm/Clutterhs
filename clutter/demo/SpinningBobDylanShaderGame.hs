
import Prelude
import qualified Prelude as P

import Graphics.UI.Clutter hiding (Nothing)
import Graphics.UI.Clutter.Gtk
import Graphics.UI.Gtk hiding (Color)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.SourceView

import Data.Maybe

data ShaderView = ShaderView Texture (Clone Texture) Shader

createShaderView stage = do
  texture <- textureNewFromFile "bob.jpg"
  reflection <- cloneNew (Just texture)
  shader <- shaderNew
  containerAddActor stage texture
  containerAddActor stage reflection
  return (ShaderView texture reflection shader)

resize (ShaderView txt refl shd) width height = do
  width <- get txt actorWidth
  height <- get txt actorHeight
  set txt [ actorX := width / 2,
            actorY := height /2 ]
  set refl [ actorX := width / 2,
             actorY := height /2 + height]

runShader (ShaderView txt cln shdr) srcBuf = do
  shaderSetIsEnabled shdr False
  src <- get srcBuf textBufferText
  shaderSetFragmentSource shdr src
  ret <- shaderCompile shdr
  if ret
     then putStrLn "Shader compile OK"
     else putStrLn "Shader compile failed"
  shaderSetIsEnabled shdr True
  actorSetShader txt (Just shdr)
  actorSetShader cln (Just shdr)

  width <- get txt actorWidth
  height <- get txt actorHeight

  set cln [ actorWidth := 300,
            actorHeight := 300,
            actorRotationAngleZ := 180,
            actorOpacity := 80,
            actorAnchorX := width / 2,
            actorAnchorY := height / 2]

  set txt [ actorWidth := 300,
            actorHeight := 300,
            actorAnchorX := width / 2,
            actorAnchorY := height / 2 ]

createShaderEditor sv@(ShaderView _ _ shdr) = do
  hbox <- hBoxNew False 10
  compileButton <- buttonNewWithLabel "Compile"

  smanager <- sourceLanguageManagerGetDefault
  shaderLang <- fmap (fromMaybe (P.error "language failed")) $
                     sourceLanguageManagerGetLanguage smanager "c"

  glslDefault <- readFile "default.glsl"

  srcBuf <- sourceBufferNewWithLanguage shaderLang
  set srcBuf [ sourceBufferHighlightSyntax := True,
               sourceBufferHighlightMatchingBrackets := True,
               textBufferText := glslDefault ]

  srcView <- sourceViewNewWithBuffer srcBuf
  set srcView [ sourceViewShowLineNumbers := True,
                sourceViewIndentOnTab := True,
                sourceViewTabWidth := 4,
                sourceViewRightMarginPosition := 80,
                sourceViewHighlightCurrentLine := True,
                sourceViewHighlightCurrentLine := True]


  scrollWin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrollWin PolicyAutomatic PolicyAutomatic

  containerAdd scrollWin srcView
  boxPackStart hbox scrollWin PackGrow 2
  boxPackStart hbox compileButton PackNatural 2

  Gtk.onClicked compileButton (runShader sv srcBuf)
  runShader sv srcBuf   --start out set

  return hbox


main = do
  initGUI
  gtkClutterInit

  window <- windowNew
  Gtk.onDestroy window Gtk.mainQuit

  embed <- clutterEmbedNew
  widgetSetSizeRequest embed 500 500

  stage <- clutterEmbedGetStage embed
  set stage [ stageColor := Color 0 0 0 255 ]

  frame <- frameNew

  pane <- vPanedNew
  set pane [panedPosition := 100]

  panedAdd1 pane embed
  panedAdd2 pane frame

  shaderView <- createShaderView stage
  editor <- createShaderEditor shaderView

  containerAdd frame editor
  containerAdd window pane

  w <- get stage actorWidth
  h <- get stage actorHeight
  resize shaderView w h

  widgetShowAll window

  mainGUI

