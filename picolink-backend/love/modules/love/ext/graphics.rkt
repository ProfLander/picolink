#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics
  #:template (#%member (#%member love graphics) _)

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
  [drawInstanced         draw-instanced]
  [drawLayer             draw-layer]
  ellipse
  [flushBatch            flush-batch]
  line
  points
  polygon
  present
  print
  printf
  rectangle
  stencil

  ; Object Creation
  [captureScreenshot     capture-screenshot]
  [newArrayImage         new-array-image]
  [newCanvas             new-canvas]
  [newCubeImage          new-cube-image]
  [newFont               new-font]
  [newImage              new-image]
  [newImageFont          new-image-font]
  [newMesh               new-mesh]
  [newParticleSystem     new-particle-system]
  [newQuad               new-quad]
  [newShader             new-shader]
  [newSpriteBatch        new-sprite-batch]
  [newText               new-text]
  [newVideo              new-video]
  [newVolumeImage        new-volume-image]
  [setNewFont            set-new-font]
  [validateShader        validate-shader]

  ; Graphics State
  [getBackgroundColor    get-background-color]
  [getBlendMode          get-blend-mode]
  [getCanvas             get-canvas]
  [getColor              get-color]
  [getDefaultFilter      get-default-filter]
  [getDepthMode          get-depth-mode]
  [getFont               get-font]
  [getFrontFaceWinding   get-front-face-winding]
  [getLineJoin           get-line-join]
  [getLineStyle          get-line-style]
  [getLineWidth          get-line-width]
  [getMeshCullMode       get-mesh-cull-mode]
  [getPointSize          get-point-size]
  [getScissor            get-scissor]
  [getShader             get-shader]
  [getStackDepth         get-stack-depth]
  [getStencilTest        get-stencil-test]
  [intersectScissor      intersect-scissor]
  [isActive              is-active]
  [isGammaCorrect        is-gamma-correct]
  [isWireframe           is-wireframe]
  reset
  [setBackgroundColor    set-background-color]
  [setBlendMode          set-blend-mode]
  [setCanvas             set-canvas]
  [setColor              set-color]
  [setColorMask          set-color-mask]
  [setDefaultFilter      set-default-filter]
  [setDepthMode          set-depth-mode]
  [setFont               set-font]
  [setFrontFaceWinding   set-front-face-winding]
  [setLineJoin           set-line-join]
  [setLineStyle          set-line-style]
  [setLineWidth          set-line-width]
  [setMeshCullMode       set-mesh-cull-mode]
  [setPointSize          set-point-size]
  [setScissor            set-scissor]
  [setShader             set-shader]
  [setStencilTest        set-stencil-test]
  [setWireframe          set-wireframe]

  ; Coordinate System
  [applyTransform        apply-transform]
  [inverseTransformPoint inverse-transform-point]
  origin
  pop
  push
  [replaceTransform      replace-transform]
  rotate
  scale
  shear
  [transformPoint        transform-point]
  translate

  ; Window
  [getDPIScale           get-dpi-scale]
  [getDimensions         get-dimensions]
  [getHeight             get-height]
  [getPixelDimensions    get-pixel-dimensions]
  [getPixelHeight        get-pixel-height]
  [getPixelWidth         get-pixel-width]
  [getWidth              get-width]

  ; System Information
  [getCanvasFormats      get-canvas-formats]
  [getImageFormats       get-image-formats]
  [getRendererInfo       get-renderer-info]
  [getStats              get-stats]
  [getSupported          get-supported]
  [getSystemLimits       get-system-limits]
  [getTextureTypes       get-texture-types]

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
  WrapMode)
