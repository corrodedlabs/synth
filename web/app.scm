
;;
;; Front end for twenty eight card game
;;
;; external libs:
;; - biwascheme: scheme interpreter for the browser
;; - three.js: 3d renderer
;;
;; docs below were taken from the references and docs avaiable in the libs above

;; handle to global js vars
(define window (js-eval "window"))
(define document (js-eval "document"))
(define body (js-ref document "body"))

;; Dimensions of the window
(define width (js-ref window "innerWidth"))
(define height (js-ref window "innerHeight"))


;; set up the scene and our camera.
;; There are a few different cameras in three.js. For now, let's use a PerspectiveCamera.
;;
;; The first attribute is the field of view. FOV is the extent of the scene that is seen on
;; the display at any given moment. The value is in degrees.
;;
;; The second one is the aspect ratio. You almost always want to use the width of the element
;; divided by the height, or you'll get the same result as when you play old movies on a
;; widescreen TV - the image looks squished.
;;
;; The next two attributes are the near and far clipping plane.
;; What that means, is that objects further away from the camera than the value of far or
;; closer than near won't be rendered.
;; You don't have to worry about this now, but you may want to use other values in your apps
;; to get better performance.
(define scene (js-new "THREE.Scene"))
(define camera (js-new "THREE.PerspectiveCamera" 75 (/ width height) 0.1 1000))

(js-set! (js-ref camera "position") "z" 5)
;; (js-invoke camera "lookAt" 0 0 0)

;; setup the webgl renderer
;; and replace the renderer's dom element with the document.body
;; this opens up the entire window to be drawn by three.js
;; In addition to creating the renderer instance, we also need to set the size at which we
;; want it to render our app. It's a good idea to use the width and height of the area we
;; want to fill with our app - in this case, the width and height of the browser window.
;; For performance intensive apps, you can also give setSize smaller values,
;; like window.innerWidth/2 and window.innerHeight/2,
;; which will make the app render at half size.
;;
;; If you wish to keep the size of your app but render it at a lower resolution, you can do
;; so by calling setSize with false as updateStyle (the third argument).
;; For example, setSize(window.innerWidth/2, window.innerHeight/2, false) will render your
;; app at half resolution, given that your <canvas> has 100% width and height.
;;
;; Last but not least, we add the renderer element to our HTML document.
;; This is a <canvas> element the renderer uses to display the scene to us.
(define renderer (js-new "THREE.WebGLRenderer"))
(js-invoke renderer "setSize" width height)
(js-invoke body "appendChild" (js-ref renderer "domElement"))


;; Lights

(define light (js-new "THREE.AmbientLight" #x404040))
(js-invoke scene "add" light)

;;
;; var light = new THREE.HemisphereLight( 0xffffbb, 0x080820, 1 );
;; scene.add( light );

(define light2 (js-new "THREE.HemisphereLight" #xffffbb 0x080820 1))
(js-invoke scene "add" light)

;; cube

;; var geometry = new THREE.BoxGeometry( 1, 1, 1 );
;; var material = new THREE.MeshBasicMaterial( {color: 0x00ff00} );
;; var cube = new THREE.Mesh( geometry, material );
;; scene.add( cube );

(define geometry (js-new "THREE.BoxGeometry" 1 1 1))
(define material (js-new "THREE.MeshBasicMaterial" (js-obj "color" #x00ff00)))
(define cube (js-new "THREE.Mesh" geometry material))
;; (js-invoke scene "add" cube)

;; Model Loaders

;; GLTFLoader
;; A loader for glTF 2.0 resources.
;;
;; glTF (GL Transmission Format) is an open format specification for efficient
;; delivery and loading of 3D content.
;; Assets may be provided either in JSON (.gltf) or binary (.glb) format. External files store
;; textures (.jpg, .png) and additional binary data (.bin).
;; A glTF asset may deliver one or more scenes, including meshes, materials, textures, skins,
;; skeletons, morph targets, animations, lights, and/or cameras. 
;;
;; Extensions:
;;
;; GLTFLoader supports the following glTF 2.0 extensions:
;;
;; KHR_draco_mesh_compression
;; KHR_materials_pbrSpecularGlossiness
;; KHR_materials_unlit
;; KHR_mesh_quantization
;; KHR_lights_punctual*
;; KHR_texture_transform**
;; MSFT_texture_dds
;;
;; * Requires physicallyCorrectLights to be enabled.
;;
;; ** UV transforms are supported, with several key limitations.
;; Transforms applied to a texture using the first UV slot (all textures except aoMap and
;; lightMap) must share the same transform, or no transfor at all. The aoMap and lightMap
;; textures cannot be transformed. No more than one transform may be used per material.
;; Each use of a texture with a unique transform will result in an additional GPU texture
;; upload. See issues #13831 and #12788 at three js repo.

;; Instantiate a new loader
(define loader (js-new "THREE.GLTFLoader"))

;; Optional: provide a DracoLoader instance to decode compressed mesh data
(define draco-loader (js-new "THREE.DRACOLoader"))
(js-invoke draco-loader "setDecoderPath" "js/draco")
(js-invoke loader "setDRACOLoader" draco-loader)

;; load a gltf resource
(define load-gltf
  (lambda (model-path)
    (define resource-loaded-callback
      (lambda (gltf-model)
	(console-log "adding" )
	(js-invoke scene "add" (js-ref gltf-model "scene"))))

    (define progress-callback
      (lambda (progress)
	(console-log (* (/ (js-ref progress "loaded")
			   (js-ref progress "total")) 100))))

    (define error-callback (lambda (error) (console-log error)))
    
    (js-invoke loader
	       "load"
	       model-path
	       (js-closure resource-loaded-callback)
	       (js-closure progress-callback)
	       (js-closure error-callback))))

(load-gltf "models/base-card.glb")

(define animate
  (lambda ()
    (js-invoke renderer "render" scene camera)
    (js-invoke window
	       "requestAnimationFrame"
	       (js-closure animate))))

(animate)
