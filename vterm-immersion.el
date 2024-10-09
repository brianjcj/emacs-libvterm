;;; vterm-immersion.el --- Extented functions for fully-featured terminal emulator -*- lexical-binding: t; -*-

;;; Commentary:

;; When `vterm-immersion-mode' is enabled, all the keys are sent to
;; vterm.  It turns emacs vterm into a standalone term app. So in
;; emacs vterm, you can login to a remote machine and use emacs
;; there just like in other regular terminal apps, without key
;; binding conflict.  You can bind some special keys to
;; `vterm-immersion-mode-map' to escape from this mode (turn off the
;; mode or just execute some local emacs command). e.g,

;; (define-key vterm-mode-map (kbd "C-S-i") #'vterm-immersion-mode)
;; (define-key vterm-immersion-mode-map (kbd "C-S-i") #'vterm-immersion-mode)

;; You can add following to your init file to use default config, or config as
;; you like.

;; (vterm-immersion-default-setup)

;; You can enable vterm-auto-immension-mode too. When it is enabled,
;; vterm-immersion mode will be activated after enter alternative screen (e.g.,
;; enter emacs, vim) and deactivated after leaving alternative screen.

;;; Code:

(require 'vterm)

;;; Immersion Mode

(defcustom vterm-auto-immension-mode t
  "when is not nil, turn on vterm-auto-immension-mode automatically
after entering alt-screen, and turn off after leaving alt-screen."
  :type  'boolean
  :group 'vterm)

(defvar vterm-immersion-mode-map
  (let ((map (make-sparse-keymap))
        (vterm-keymap-exceptions '()))
    (vterm--exclude-keys map vterm-keymap-exceptions)
    (define-key map (kbd "C-]")                 #'vterm--self-insert)
    (define-key map (kbd "M-<")                 #'vterm--self-insert)
    (define-key map (kbd "M->")                 #'vterm--self-insert)
    (define-key map [tab]                       #'vterm-send-tab)
    (define-key map (kbd "TAB")                 #'vterm-send-tab)
    (define-key map [backtab]                   #'vterm--self-insert)
    (define-key map [backspace]                 #'vterm-send-backspace)
    (define-key map (kbd "DEL")                 #'vterm-send-backspace)
    (define-key map [delete]                    #'vterm-send-delete)
    (define-key map [M-backspace]               #'vterm-send-meta-backspace)
    (define-key map (kbd "M-DEL")               #'vterm-send-meta-backspace)
    (define-key map [C-backspace]               #'vterm-send-meta-backspace)
    (define-key map [return]                    #'vterm-send-return)
    (define-key map (kbd "RET")                 #'vterm-send-return)
    (define-key map [C-left]                    #'vterm--self-insert)
    (define-key map [M-left]                    #'vterm--self-insert)
    (define-key map [C-right]                   #'vterm--self-insert)
    (define-key map [M-right]                   #'vterm--self-insert)
    (define-key map [C-up]                      #'vterm--self-insert)
    (define-key map [C-down]                    #'vterm--self-insert)
    (define-key map [M-up]                      #'vterm--self-insert)
    (define-key map [M-down]                    #'vterm--self-insert)
    (define-key map [left]                      #'vterm--self-insert)
    (define-key map [right]                     #'vterm--self-insert)
    (define-key map [up]                        #'vterm--self-insert)
    (define-key map [down]                      #'vterm--self-insert)
    (define-key map [prior]                     #'vterm--self-insert)
    (define-key map [S-prior]                   #'scroll-down-command)
    (define-key map [next]                      #'vterm--self-insert)
    (define-key map [S-next]                    #'scroll-up-command)
    (define-key map [home]                      #'vterm--self-insert)
    (define-key map [end]                       #'vterm--self-insert)
    (define-key map [C-home]                    #'vterm--self-insert)
    (define-key map [C-end]                     #'vterm--self-insert)
    (define-key map [escape]                    #'vterm--self-insert)
    (define-key map [remap yank]                #'vterm-yank)
    (define-key map [remap xterm-paste]         #'vterm-xterm-paste)
    (define-key map [remap yank-pop]            #'vterm-yank-pop)
    (define-key map [remap mouse-yank-primary]  #'vterm-yank-primary)
    (define-key map [down-mouse-1]              #'vterm-immersion-mouse-drag-region)
    (define-key map [down-mouse-3]              #'vterm-immersion-mouse-down-3)
    (define-key map [wheel-up]                  #'vterm-immersion-mwheel-scroll)
    (define-key map [wheel-down]                #'vterm-immersion-mwheel-scroll)
    (define-key map (kbd "C-SPC")               #'vterm--self-insert)
    (define-key map (kbd "S-SPC")               #'vterm-send-space)
    (define-key map (kbd "C-_")                 #'vterm--self-insert)
    (define-key map [remap undo]                #'vterm-undo)
    (define-key map (kbd "M-.")                 #'vterm--self-insert)
    (define-key map (kbd "M-,")                 #'vterm--self-insert)
    (define-key map (kbd "C-l")                 #'vterm-clear)
    (define-key map (kbd "C-\\")                #'vterm--self-insert)
    (define-key map [remap self-insert-command] #'vterm--self-insert)
    map))

(defun vterm--enter-immersion-mode ()
  (when vterm-copy-mode
    (vterm-copy-mode-done))
  (setq vterm-immersion-go-when-copy-mode-off nil)
  (use-local-map nil))

(defun vterm--exit-immersion-mode ()
  (use-local-map vterm-mode-map))

(define-minor-mode vterm-immersion-mode
  "Toggle `vterm-immersion-mode'.

When `vterm-copy-mode' is enabled, all the keys are sent to
vterm.  It turns emacs vterm into a standalone term app. So in
emacs vterm, you can login to a remote machine and use emacs
there just like in other regular terminal apps, without key
binding conflict.  You can bind some special keys to
`vterm-immersion-mode-map' to escape from this mode (turn off the
mode or just execute some local emacs command). e.g,

(define-key vterm-immersion-mode-map (kbd \"<f12>\") #'switch-to-buffer)
(define-key vterm-immersion-mode-map (kbd \"C-<f12>\") #'vterm-immersion-mode)
(define-key vterm-mode-map (kbd \"C-<f12>\") #'vterm-immersion-mode)
"
  :group 'vterm
  :lighter " VTermImmersion"
  :keymap vterm-immersion-mode-map
  (if (equal major-mode 'vterm-mode)
      (if vterm-immersion-mode
          (vterm--enter-immersion-mode)
        (vterm--exit-immersion-mode))
    (user-error "You cannot enable vterm-immersion-mode outside vterm buffers")))

(defvar-local vterm-immersion-go-when-copy-mode-off nil)

(defun vterm-immersion-copy-mode-handle()
  (unless vterm-copy-mode
    (when vterm-immersion-go-when-copy-mode-off
      (vterm-immersion-mode))))

(add-hook 'vterm-copy-mode-hook 'vterm-immersion-copy-mode-handle)

(defun vterm-immersion-exit-and-go-copy-mode()
  (interactive)
  (vterm-immersion-mode -1)
  (setq vterm-immersion-go-when-copy-mode-off t)
  (vterm-copy-mode))

;; TODO: find a better name
(defun vterm-read-and-send-command(cmd)
  (interactive "scmd: ")
  (message cmd)
  (vterm-insert cmd))

;; mouse support
(defun vterm-immersion-mwheel-scroll (event &optional arg)
  "mouse whell"
  (interactive (list last-input-event current-prefix-arg))
  (if (vterm--get-on-altscreen vterm--term)
      (vterm-immersion-mwheel-scroll-on-altscreen event arg)
    (mwheel-scroll event arg)))

(defun vterm-immersion-mwheel-scroll-on-altscreen (event &optional arg)
  "mouse whell"
  (interactive (list last-input-event current-prefix-arg))
  (let ((button (mwheel-event-button event))
        (button-no 4)
        (mod (vterm-get-event-modifier event)))
    (pcase button
      ('wheel-up (setq button-no 4))
      ('wheel-down (setq button-no 5)))
    (let ((pos (posn-col-row (event-end event))))
      (vterm--mouse-move vterm--term (cdr pos) (car pos) mod)
      (vterm--mouse-button vterm--term button-no t mod))))

(defun vterm-get-mouse-num (event)
  (pcase event
    ((or (and (let mouse-type (event-basic-type event))
              (let (rx string-start "mouse-"
                       (let key-num (one-or-more
                                     (any (?0 . ?9))))
                       string-end)
                (symbol-name mouse-type))
              (let (and (pred (<= 1))
                        (pred (>= 11))
                        mouse-num)
                (string-to-number key-num)))
         (and (let 'wheel-up (event-basic-type mouse))
              (let mouse-num 4))
         (and (let 'wheel-down (event-basic-type mouse))
              (let mouse-num 5))
         (and (let 'wheel-right (event-basic-type mouse))
              (let mouse-num 6))
         (and (let 'wheel-left (event-basic-type mouse))
              (let mouse-num 7)))
     mouse-num)
    (t 1)))

(defun vterm-get-event-modifier (event)
  (let ((m (event-modifiers event))
        (mod 0))
    (when (memq 'control m)
      (setq m (+ m 4)))
    (when (memq 'meta m)
      (setq m (+ m 2)))
    (when (memq 'shift m)
      (setq m (+ m 1)))
    mod))

(defun vterm-immersion-mouse-drag-region (event)
  ""
  (interactive "e")
  (if (vterm--get-on-altscreen vterm--term)
      (vterm-immersion-mouse-down-on-altscreen event)
    (mouse-drag-region event)))

(defun vterm-immersion-mouse-down-3 (event)
  ""
  (interactive "e")
  (when (vterm--get-on-altscreen vterm--term)
      (vterm-immersion-mouse-down-on-altscreen event)))

(defun vterm-immersion-mouse-down-on-altscreen (event)
  ""
  (interactive "e")
  ;; (print (event-basic-type event))
  (let ((pos (posn-col-row (event-end event)))
        (mouse-num (vterm-get-mouse-num event))
        (mod (vterm-get-event-modifier event))
        ev)
    (vterm--mouse-move vterm--term (cdr pos) (car pos) mod)
    (vterm--mouse-button vterm--term mouse-num t mod)
    (track-mouse
      (while
          (progn
            (setq ev (read--potential-mouse-event))
            (mouse-movement-p ev))
        (setq mod (vterm-get-event-modifier ev))
        (let ((pos (posn-col-row (event-end ev))))
          (vterm--mouse-move vterm--term (cdr pos) (car pos) mod))))
    (vterm--mouse-button vterm--term mouse-num nil mod)))

(defun vterm-immersion-on-altscreen-change (on-altscreen)
  (when vterm-auto-immension-mode
    (vterm-immersion-mode (if on-altscreen 1 -1))))

(add-hook 'vterm-on-altscreen-changed-hook 'vterm-immersion-on-altscreen-change)

(defun vterm-immersion-default-setup()
  ;; (define-key vterm-immersion-mode-map (kbd "<f12>") #'switch-to-buffer)
  ;; (define-key vterm-immersion-mode-map (kbd "C-<f12>") #'vterm-immersion-mode)
  ;; (define-key vterm-mode-map (kbd "C-<f12>") #'vterm-immersion-mode)

  (define-key vterm-immersion-mode-map (kbd "C-S-b") #'switch-to-buffer)
  (global-set-key (kbd "C-S-b") #'switch-to-buffer)

  (define-key vterm-mode-map (kbd "C-S-i") #'vterm-immersion-mode)
  (define-key vterm-immersion-mode-map (kbd "C-S-i") #'vterm-immersion-mode)

  (define-key vterm-immersion-mode-map (kbd "C-S-v") #'yank) ;; paste
  (define-key vterm-immersion-mode-map (kbd "C-S-c") #'kill-ring-save) ;; copy
  (global-set-key (kbd "C-S-v") #'yank) ;; paste
  (global-set-key (kbd "C-S-c") #'kill-ring-save) ;; copy

  (define-key vterm-immersion-mode-map (kbd "M-X") #'execute-extended-command)
  (define-key vterm-mode-map (kbd "M-X") #'execute-extended-command)
  (global-set-key (kbd "M-X") #'execute-extended-command)

  (define-key vterm-immersion-mode-map (kbd "C-S-t") #'vterm-immersion-exit-and-go-copy-mode)
  (define-key vterm-copy-mode-map (kbd "C-S-t") #'vterm-copy-mode)
  (define-key vterm-mode-map (kbd "C-S-t") #'vterm-copy-mode)

  (define-key vterm-mode-map (kbd "M-?") #'vterm-read-and-send-command)
  (define-key vterm-immersion-mode-map (kbd "M-?") #'vterm-read-and-send-command)

  (define-key vterm-mode-map (kbd "M-:") #'eval-expression)
  )

(provide 'vterm-immersion)

;;; vterm-immersion.el ends here
