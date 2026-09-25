#lang picolink/intrinsic

; Types
Canvas
Drawable
Font
Framebuffer
Image
Mesh
ParticleSystem
PixelEffect
Quad
Shader
SpriteBatch
Text
Texture
Video

; Drawing
arc
circle
clear
discard
draw
drawInstanced
drawLayer
ellipse
flushBatch
line
points
polygon
present
print
printf
rectangle
stencil

; Object Creation
captureScreenshot
newArrayImage
newCanvas
newCubeImage
newFont
newImage
newImageFont
newMesh
newParticleSystem
newQuad
newShader
newSpriteBatch
newText
newVideo
newVolumeImage
setNewFont
validateShader

; Graphics State
getBackgroundColor
getBlendMode
getCanvas
getColor
getDefaultFilter
getDepthMode
getFont
getFrontFaceWinding
getLineJoin
getLineStyle
getLineWidth
getMeshCullMode
getPointSize
getScissor
getShader
getStackDepth
getStencilTest
intersectScissor
isActive
isGammaCorrect
isWireframe
reset
setBackgroundColor
setBlendMode
setCanvas
setColor
setColorMask
setDefaultFilter
setDepthMode
setFont
setFrontFaceWinding
setLineJoin
setLineStyle
setLineWidth
setMeshCullMode
setPointSize
setScissor
setShader
setStencilTest
setWireframe

; Coordinate System
applyTransform
inverseTransformPoint
origin
pop
push
replaceTransform
rotate
scale
shear
transformPoint
translate

; Window
getDPIScale
getDimensions
getHeight
getPixelDimensions
getPixelHeight
getPixelWidth
getWidth

; System Information
getCanvasFormats
getImageFormats
getRendererInfo
getStats
getSupported
getSystemLimits
getTextureTypes

; Enums
AlignMode
ArcType
AttributeDataType
BlendAlphaMode
BlendMode
BufferDataUsage
CanvasFormat
CompareMode
CullMode
DrawMode
FilterMode
GraphicsFeature
GraphicsLimit
IndexDataType
LineJoin
LineStyle
MeshDrawMode
MipmapMode
PixelFormat
StackType
StencilAction
TextureType
VertexAttributeStep
VertexWinding
WrapMode
