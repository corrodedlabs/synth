
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

(define append-to-body
  (lambda (element) (js-invoke body "appendChild" element)))

(define replace-body
  (lambda (element)
    (let ((app (js-invoke document "getElementById" "app")))
      (js-set! app "innerHTML" "")
      (js-invoke app "appendChild" element))))


;; Dimensions of the window
(define width (js-ref window "innerWidth"))
(define height (js-ref window "innerHeight"))

(define π (js-eval "Math.PI"))

;; Representing position

(define-record-type position (fields x y z))

(define set-position (lambda (obj pos)
		       (js-invoke (js-ref obj "position")
				  "set"
				  (position-x pos)
				  (position-y pos)
				  (position-z pos))))

;; some handy constructors for setting position
(define (z val) (make-position 0 0 val))
(define (y val) (make-position 0 val 0))
(define (x val) (make-position val 0 0))

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

(set-position camera (make-position 0 10 30))
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

(define attach-renderer
  (lambda (renderer body)
    (js-invoke renderer "setSize" width height)
    (js-invoke body "appendChild" (js-ref renderer "domElement"))))


(define add-to-scene (lambda (scene obj) (js-invoke scene "add" obj)))

;; Lights

;; Buld

;; // ref for lumens: http://www.power-sure.com/lumens.htm
;; var bulbLuminousPowers = {
;; 			  "110000 lm (1000W)": 110000,
;; 			  "3500 lm (300W)": 3500,
;; 			  "1700 lm (100W)": 1700,
;; 			  "800 lm (60W)": 800,
;; 			  "400 lm (40W)": 400,
;; 			  "180 lm (25W)": 180,
;; 			  "20 lm (4W)": 20,
;; 			  "Off": 0
;; 			  };

;; // ref for solar irradiances: https://en.wikipedia.org/wiki/Lux
;; var hemiLuminousIrradiances = {
;; 			       "0.0001 lx (Moonless Night)": 0.0001,
;; 			       "0.002 lx (Night Airglow)": 0.002,
;; 			       "0.5 lx (Full Moon)": 0.5,
;; 			       "3.4 lx (City Twilight)": 3.4,
;; 			       "50 lx (Living Room)": 50,
;; 			       "100 lx (Very Overcast)": 100,
;; 			       "350 lx (Office Room)": 350,
;; 			       "400 lx (Sunrise/Sunset)": 400,
;; 			       "1000 lx (Overcast)": 1000,
;; 			       "18000 lx (Daylight)": 18000,
;; 			       "50000 lx (Direct Sun)": 50000
;; 			       };



(define light (js-new "THREE.AmbientLight" #xffffff))
(add-to-scene scene light)

;; Hemisphere Lights
;;
;; var light = new THREE.HemisphereLight( 0xffffbb, 0x080820, 1 );
;; scene.add( light );

(define light2 (js-new "THREE.HemisphereLight" #xffffbb 0x080820 1))
(add-to-scene scene light)


;; SpotLight
;;
;; This light gets emitted from a single point in one direction, along a cone that increases in size the further from the light it gets.

;; This light can cast shadows - see the SpotLightShadow page for details.

;; Code Example
;; // white spotlight shining from the side, casting a shadow

;; var spotLight = new THREE.SpotLight( 0xffffff );
;; spotLight.position.set( 100, 1000, 100 );

;; spotLight.castShadow = true;

;; spotLight.shadow.mapSize.width = 1024;
;; spotLight.shadow.mapSize.height = 1024;

;; spotLight.shadow.camera.near = 500;
;; spotLight.shadow.camera.far = 4000;
;; spotLight.shadow.camera.fov = 30;

;; scene.add( spotLight );

(define spotlight (js-new "THREE.SpotLight" #xffffff))
(set-position spotlight (make-position 100 1000 100))

;; shadow props


(let ((shadow-map-size-obj  (js-ref (js-ref spotlight "shadow") "mapSize"))
      (shadow-camera (js-ref (js-ref spotlight "shadow") "camera")))
  (js-set! spotlight "castShadow" #t)
  (js-set! shadow-map-size-obj "width" 1024)
  (js-set! shadow-camera "near" 500)
  (js-set! shadow-camera "far" 4000)
  (js-set! shadow-camera "fov" 30))

(add-to-scene scene spotlight)

;; Floor material
;; floorMat = new THREE.MeshStandardMaterial( {
;; 					roughness: 0.8,
;; 					color: 0xffffff,
;; 					metalness: 0.2,
;; 					bumpScale: 0.0005
;; 				} );

(define floor-material (js-new "THREE.MeshStandardMaterial"
			       (js-obj "roughness" 0.8
				       "color" #xffffff
				       "metalness" 0.2
				       "bumpScale" 0.0005)))

;; Textures

;; var textureLoader = new THREE.TextureLoader();
;; 				textureLoader.load( "textures/hardwood2_diffuse.jpg", function ( map ) {

;; 					map.wrapS = THREE.RepeatWrapping;
;; 					map.wrapT = THREE.RepeatWrapping;
;; 					map.anisotropy = 4;
;; 					map.repeat.set( 10, 24 );
;; 					map.encoding = THREE.sRGBEncoding;
;; 					floorMat.map = map;
;; 					floorMat.needsUpdate = true;

;; 	} );
;; 				textureLoader.load( "textures/hardwood2_bump.jpg", function ( map ) {

;; 					map.wrapS = THREE.RepeatWrapping;
;; 					map.wrapT = THREE.RepeatWrapping;
;; 					map.anisotropy = 4;
;; 					map.repeat.set( 10, 24 );
;; 					floorMat.bumpMap = map;
;; 					floorMat.needsUpdate = true;

;; 	} );
;; 				textureLoader.load( "textures/hardwood2_roughness.jpg", function ( map ) {

;; 					map.wrapS = THREE.RepeatWrapping;
;; 					map.wrapT = THREE.RepeatWrapping;
;; 					map.anisotropy = 4;
;; 					map.repeat.set( 10, 24 );
;; 					floorMat.roughnessMap = map;
;; 					floorMat.needsUpdate = true;

;; 	} );

(define texture-loader (js-new "THREE.TextureLoader"))

(define +repeat-wrapping+ (js-eval "THREE.RepeatWrapping"))
(define +s-rgbe-encoding+ (js-eval "THREE.sRGBEncoding"))

(define load-texture
  (lambda (texture-path)
    (call/cc (lambda (k)
	       (js-invoke texture-loader "load" texture-path
			  (js-closure (lambda (map)
					;; (js-set! map "wrapS" +repeat-wrapping+)
					;; (js-set! map "wrapT" +repeat-wrapping+)
					;; (js-set! map "anisotropy" 4)
					;; (js-invoke (js-ref map "repeat") "set" 10 24)
					(k map))))))))

;; setup textures for the floor
(let ((diffuse-map (load-texture "textures/hardwood2_diffuse.jpg"))
      (bump-map (load-texture "textures/hardwood2_bump.jpg"))
      (roughness-map (load-texture "textures/hardwood2_roughness.jpg")))
  (js-set! diffuse-map "encoding" +s-rgbe-encoding+)
  (js-set! floor-material "map" diffuse-map)
  (js-set! floor-material "bumpMap" bump-map)
  (js-set! floor-material "roughnessMap" roughness-map)
  (js-set! floor-material "needsUpdate" #t))

;; var floorGeometry = new THREE.PlaneBufferGeometry( 20, 20 );
;; var floorMesh = new THREE.Mesh( floorGeometry, floorMat );
;; floorMesh.receiveShadow = true;
;; floorMesh.rotation.x = - Math.PI / 2.0;
;; scene.add( floorMesh );

(console-log floor-material)

(define floor-geometry (js-new "THREE.PlaneBufferGeometry" 60 60))
(define floor-mesh (js-new "THREE.Mesh" floor-geometry floor-material))
(js-set! floor-mesh "receiveShadow" #t)
(console-log "pi is" π)
(js-set! (js-ref floor-mesh "rotation") "x" (- (/ π 2.0)))

(set-position floor-mesh (make-position 0 0 0))
(add-to-scene scene floor-mesh)


;; Geometries

;; PlaneGeometry
;; A class for generating plane geometries


;; Code Example
;; var geometry = new THREE.PlaneGeometry( 5, 20, 32 );
;; var material = new THREE.MeshBasicMaterial( {color: 0xffff00, side: THREE.DoubleSide} );
;; var plane = new THREE.Mesh( geometry, material );
;; scene.add( plane );
;; Constructor
;; PlaneGeometry(width : Float, height : Float, widthSegments : Integer, heightSegments : Integer)
;; width — Width along the X axis. Default is 1.
;; height — Height along the Y axis. Default is 1.
;; widthSegments — Optional. Default is 1.
;; heightSegments — Optional. Default is 1.

;; Properties
;; See the base Geometry class for common properties.

;; .parameters
;; An object with a property for each of the constructor parameters. Any modification after instantiation does not change the geometry.

;; Methods
;; See the base Geometry class for common methods.

(define create-plane
  (case-lambda
   ((width height) (create-plane width height 1 1))
   ((width height width-segments height-segments)
    (js-new "THREE.PlaneGeometry" width height width-segments height-segments))))

;; (let ((plane-geometry (create-plane 200 200)))
;;   )

;; (add-to-scene scene )

;; cube

;; var geometry = new THREE.BoxGeometry( 1, 1, 1 );
;; var material = new THREE.MeshBasicMaterial( {color: 0x00ff00} );
;; var cube = new THREE.Mesh( geometry, material );
;; scene.add( cube );

(define geometry (js-new "THREE.BoxGeometry" 1 1 1))
(define material (js-new "THREE.MeshStandardMaterial" (js-obj "color" #xff0000)))
(define cube (js-new "THREE.Mesh" geometry material))

(set-position cube (make-position 0 0 0))
(add-to-scene scene cube)

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

(js-eval "window.isMesh = function(x) { return (x instanceof THREE.Mesh);}")

;; load a gltf resource
(define load-gltf
  (lambda (model-path)
    (define resource-loaded-callback
      (lambda (gltf-model)
	(console-log "adding" )
	(let ((mesh (js-ref gltf-model "scene")))
	  (js-set! (js-ref mesh "rotation") "x" (js-eval "Math.PI"))
	  (js-set! (js-ref mesh "position") "x" 0)
	  (js-set! (js-ref mesh "position") "y" 5)
	  (js-set! (js-ref mesh "position") "z" 21)
	  (js-invoke (js-ref mesh "scale") "set" 0.5 0.5 0.5)
	  (js-invoke scene "add" mesh)

	  (let ((mesh2 (js-invoke mesh "clone"))
		(card (load-texture "textures/nine.jpeg")))
	    (define n 2)
	    (let* ((scene-data  (js-array->list (js-ref mesh2 "children")))
		   (card-mesh (caddr scene-data)))
	      (let* ((card-face (js-array->list (js-ref card-mesh "children")))
		     (child-material (js-ref (cadr card-face) "material")))
		(js-set! child-material "map" card)
		(js-set! (js-ref child-material "map") "needsUpdate" #t)
		(js-set! child-material "needsUpdate" #t)
		
		(set-position mesh2 (make-position 2 5 21))
		(add-to-scene scene mesh2)))))))

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


;; renderer = new THREE.WebGLRenderer();
;; renderer.physicallyCorrectLights = true;
;; renderer.outputEncoding = THREE.sRGBEncoding;
;; renderer.shadowMap.enabled = true;
;; renderer.toneMapping = THREE.ReinhardToneMapping;
;; renderer.setPixelRatio( window.devicePixelRatio );
;; renderer.setSize( window.innerWidth, window.innerHeight );

(define animate
  (lambda ()
    (js-set! renderer "physicallyCorrectLights" #t)
    (js-set! renderer "outputEncoding" +s-rgbe-encoding+)
    (js-set! (js-ref renderer "shadowMap") "enabled" #t)
    (js-set! renderer "toneMapping" (js-eval "THREE.ReinhardToneMapping"))
    (js-invoke renderer "render" scene camera)
    (js-invoke window
	       "requestAnimationFrame"
	       (js-closure animate))))

;; (animate)

;; Websockets

(define *socket* (js-new  "WebSocket" "ws://localhost:8081"))

(define *pending-continuation* #f)

(define *ws-connected?* #f)
(define *pending-messages* '())

(define setup-socket
  (lambda (socket)
    (define on-close
      (lambda (event)
	(console-log "connection closed" event)))

    (define on-error
      (lambda (event)
	(console-log "error occured in websocket" event)))

    (define on-message
      (lambda (event)
	(console-log "new message has arrived" (js-ref event "data"))
	(if *pending-continuation* (*pending-continuation* #t) #f)))
    
    (define on-open
      (lambda (event)
	(console-log "connection opened")
	(set! *ws-connected?* #t)
	(let loop ((pending-msg-conts *pending-messages*))
	  (cond
	   ((null? pending-msg-conts) #f)
	   (else (begin  ((car pending-msg-conts) #t)
			 (loop (cdr pending-msg-conts))))))))
    
    (js-set! socket "onclose" (js-closure on-close))
    (js-set! socket "onerror" (js-closure on-error))
    (js-set! socket "onmessage" (js-closure on-message))
    (js-set! socket "onopen" (js-closure on-open))))

(setup-socket *socket*)

(define send-message!
  (case-lambda
   ((message) (send-message! *socket* message))
   ((socket message)
    (call/cc (lambda (k)
	       (console-log "sending message" (format #f "~s" message))
	       (if *ws-connected?*
		   (js-invoke socket "send" (format #f "~s" message))
		   (append *pending-messages* k)))))))

(define connect-user
  (lambda (user-email pic-url)
    (send-message! `(connect-user ,user-email ,pic-url))))


(define create-room
  (lambda (email room-name)
    (call/cc (lambda (k)
	       (send-message! `(make-room ,email ,room-name))
	       (console-log "message sent")
	       (set! *pending-continuation* k)
	       #f))))

;; Ui setup

(define button-class "bg-transparent hover:bg-blue-500 text-blue-700 font-semibold 
                      hover:text-white py-2 px-4 border border-blue-500 
                      hover:border-transparent rounded ")

(define (button id text)
  `(div class
	"w-64 text-center text-grey-darker bg-grey-light px-4 py-2 m-2 "
	(button id ,id class ,button-class  ,text)))

(define *events* '())

(define new-event
  (lambda (thunk)
    (set! *events* (append *events* (list thunk)))))

(define start-render
  (lambda ()
    (let ((event ((car *events*))))
      (set! *events* (cdr *events*))
      (render-page event))))

(define next-event
  (lambda ()
    (call/cc (lambda (k)
	       (new-event (lambda () (k #f)))
	       (start-render)))))

;; Sign In page

;; <div class="g-signin2" data-onsuccess="onSignIn"></div>

(define-record-type element (fields html on-load))

(define sign-in-element
  (lambda ()
    (make-element
     (element-new
      '(div class
	    "container mx-auto h-full flex flex-col h-screen items-center justify-center"
	    (div#sign-in)))
     (lambda ()
       (define on-sign-in
	 (js-closure
	  (lambda (user-info)
	    (let* ((profile (js-invoke user-info "getBasicProfile"))
		   (token (js-ref (js-invoke user-info "getAuthResponse") "id_token"))
		   (email (js-invoke profile "getEmail"))
		   (name (js-invoke profile "getName"))
		   (pic-url (js-invoke profile "getImageUrl")))
	      (render-page 'home `((token . ,token)
				   (email . ,email)
				   (name . ,name)
				   (photo-url . ,pic-url)))))))

       (define on-fail (js-closure (lambda (error) (console-log error))))
       
       (js-set! window
		"renderButton"
		(js-closure (lambda ()
			      (js-invoke (js-ref (js-ref window "gapi") "signin2")
					 "render"
					 "sign-in"
					 (js-obj "scope" "profile email"
						 "width" 240
						 "height" 50
						 "longtitle" #t
						 "onsuccess" on-sign-in
						 "onfailure" on-fail)))))))))


(define container
  '(div class "container mx-auto h-full flex flex-col h-screen items-center justify-center"))

(define (assget key alist)
  (cond
   ((assoc alist key) => cdr)
   (else #f)))

;; Home page
;;
;; The options available are:
;; * create a game room
;; * join a game room
;;

(define home-element
  (lambda (data)
    (make-element (element-new
		   `(,@container
		     ,(button "create" "Create a Game room")
		     ,(button "join" "Join a Game room")))
		  (lambda ()
		    (connect-user (assget data 'email)
				  (assget data 'photo-url))
		    (add-handler! "#create" "click" (lambda (event)
						      (console-log "create a game room")
						      (render-page 'create-room data)))
		    (add-handler! "#join" "click" (lambda (event)
						    (console-log "join a game room")
						    (render-page 'join-room #f)))))))

(define-record-type player (fields name email photo-url))

(define create-room-element
  (lambda (data)
    (let ((name (cdr (assoc 'name data)))
	  (email (cdr (assoc 'email data)))
	  (photo-url (cdr (assoc 'photo-url data))))
      (make-element
       (element-new
	`(,@container
	  (form class "w-full max-w-sm"
		(div class "flex items-center border-b border-b-2 border-teal-500 py-2"
		     (input#game-room-name
		      class "appearance-none bg-transparent border-none w-full 
                             text-gray-700 mr-3 py-1 px-2 leading-tight focus:outline-none"
		      type "text"
		      placeholder "Enter Game room name"
		      aria-label "Game room name")
		     (button#create-room
		      class "flex-shrink-0 bg-teal-500 hover:bg-teal-700 border-teal-500
                         hover:border-teal-500 text-sm border-4 text-white py-1 px-2
                         rounded"
		      type "button"
		      "Create Room")
		     (button#go-back
		      class "flex-shrink-0 border-transparent border-4 text-teal-500
                         hover:text-teal-800 text-sm py-1 px-2 rounded"
		      type "button"
		      "Go back")))))
       (lambda ()
	 (add-handler! "#create-room"
		       "click"
		       (lambda (event)
			 (let ((room-name (get-content "#game-room-name")))
			   (cond
			    ((create-room email room-name)			   
			     (begin (console-log "now rendering page")
				    (render-page 'game-room
						 (cons room-name
						       (list (make-player name email photo-url))))))
			    (else #f)))))
	 (add-handler! "#go-back"
		       "click"
		       (lambda (event)
			 (console-log "go back")
			 (render-page 'home))))))))

(define add-click-handler
  (lambda (id f)
    (add-handler! (symbol->string id) "click" f)))

(define game-room-element
  (case-lambda
   ((data)
    (game-room-element (car data) (cdr data)))
   ((game-room-name players)
    (define player-view (lambda (player)
			  (console-log "player is " player)
			  `(div class "flex items-center"
				(img class "w-10 h-10 rounded-full mr-4"
				     alt "Avatar"
				     src ,(player-photo-url player))
				(div.text-sm
				 (p class "text-gray-900 leading-none"
				    ,(player-name player))))))

    (define start-game-button (if (= (length players) 4) (button "Start Game") '(div)))
    
    (make-element
     (element-new `(,@container (div class "font-bold text-2xl mb-2" "My Game Room")
				(div class "text-lg mb-2" "Players in this room")
				(div class "flex items-center" ,@(map player-view players))
				(button#add-bot "Add Bot")
				,start-game-button))
     (lambda ()
       (add-click-handler '#add-bot
			  (lambda (event)
			    (send-message! `(add-bot-to-room ,game-room-name)))))))))

(define render-page
  (case-lambda
   ((page) (render-page page #f))
   ((page data)
    (console-log "rendering page " page "with data" data)
    (let ((element (case page
		     ((sign-in) (sign-in-element))
		     ((home) (home-element data))
		     ((create-room) (create-room-element data))
		     ((game-room) (game-room-element data)))))
      (replace-body (element-html element))
      ((element-on-load element))))))

(render-page 'sign-in)
