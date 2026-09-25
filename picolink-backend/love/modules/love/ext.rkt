#lang picolink/s-lua

#:provide [set-love-draw
           set-love-error-handler
           set-love-load
           set-love-low-memory
           set-love-quit
           set-love-run
           set-love-thread-error
           set-love-update

           set-love-directory-dropped
           set-love-display-rotated
           set-love-file-dropped
           set-love-focus
           set-love-mouse-focus
           set-love-resize
           set-love-visible

           set-love-key-pressed
           set-love-key-released
           set-love-text-edited
           set-love-text-input

           set-love-mouse-moved
           set-love-mouse-pressed
           set-love-mouse-released
           set-love-wheel-moved

           set-love-gamepad-axis
           set-love-gamepad-pressed
           set-love-gamepad-released
           set-love-joystick-added
           set-love-joystick-axis
           set-love-joystick-hat
           set-love-joystick-pressed
           set-love-joystick-released
           set-love-joystick-removed

           set-love-touch-moved
           set-love-touch-pressed
           set-love-touch-released]

; General

(#%function (set-love-draw f)
            (#%block
             (#%assign [(#%member love draw)] [f])))

(#%function (set-love-error-handler f)
            (#%block
             (#%assign [(#%member love errorhandler)] [f])))

(#%function (set-love-load f)
            (#%block
             (#%assign [(#%member love load)] [f])))

(#%function (set-love-low-memory f)
            (#%block
             (#%assign [(#%member love lowmemory)] [f])))

(#%function (set-love-quit f)
            (#%block
             (#%assign [(#%member love quit)] [f])))

(#%function (set-love-run f)
            (#%block
             (#%assign [(#%member love run)] [f])))

(#%function (set-love-thread-error f)
            (#%block
             (#%assign [(#%member love threaderror)] [f])))

(#%function (set-love-update f)
            (#%block
             (#%assign [(#%member love update)] [f])))

; Window

(#%function (set-love-directory-dropped f)
            (#%block
             (#%assign [(#%member love directorydropped)] [f])))

(#%function (set-love-display-rotated f)
            (#%block
             (#%assign [(#%member love displayrotated)] [f])))

(#%function (set-love-file-dropped f)
            (#%block
             (#%assign [(#%member love filedropped)] [f])))

(#%function (set-love-focus f)
            (#%block
             (#%assign [(#%member love focus)] [f])))

(#%function (set-love-mouse-focus f)
            (#%block
             (#%assign [(#%member love mousefocus)] [f])))

(#%function (set-love-resize f)
            (#%block
             (#%assign [(#%member love resize)] [f])))

(#%function (set-love-visible f)
            (#%block
             (#%assign [(#%member love visible)] [f])))

; Keyboard

(#%function (set-love-key-pressed f)
            (#%block
             (#%assign [(#%member love keypressed)] [f])))

(#%function (set-love-key-released f)
            (#%block
             (#%assign [(#%member love keyreleased)] [f])))

(#%function (set-love-text-edited f)
            (#%block
             (#%assign [(#%member love textedited)] [f])))

(#%function (set-love-text-input f)
            (#%block
             (#%assign [(#%member love textinput)] [f])))

; Mouse

(#%function (set-love-mouse-moved f)
            (#%block
             (#%assign [(#%member love mousemoved)] [f])))

(#%function (set-love-mouse-pressed f)
            (#%block
             (#%assign [(#%member love mousepressed)] [f])))

(#%function (set-love-mouse-released f)
            (#%block
             (#%assign [(#%member love mousereleased)] [f])))

(#%function (set-love-wheel-moved f)
            (#%block
             (#%assign [(#%member love wheelmoved)] [f])))

; Joystick

(#%function (set-love-gamepad-axis f)
            (#%block
             (#%assign [(#%member love gamepadaxis)] [f])))

(#%function (set-love-gamepad-pressed f)
            (#%block
             (#%assign [(#%member love gamepadpressed)] [f])))

(#%function (set-love-gamepad-released f)
            (#%block
             (#%assign [(#%member love gamepadreleased)] [f])))

(#%function (set-love-joystick-added f)
            (#%block
             (#%assign [(#%member love joystickadded)] [f])))

(#%function (set-love-joystick-axis f)
            (#%block
             (#%assign [(#%member love joystickaxis)] [f])))

(#%function (set-love-joystick-hat f)
            (#%block
             (#%assign [(#%member love joystickhat)] [f])))

(#%function (set-love-joystick-pressed f)
            (#%block
             (#%assign [(#%member love joystickpressed)] [f])))

(#%function (set-love-joystick-released f)
            (#%block
             (#%assign [(#%member love joystickreleased)] [f])))

(#%function (set-love-joystick-removed f)
            (#%block
             (#%assign [(#%member love joystickremoved)] [f])))

; Touch

(#%function (set-love-touch-moved f)
            (#%block
             (#%assign [(#%member love touchmoved)] [f])))

(#%function (set-love-touch-pressed f)
            (#%block
             (#%assign [(#%member love touchpressed)] [f])))

(#%function (set-love-touch-released f)
            (#%block
             (#%assign [(#%member love touchreleased)] [f])))
