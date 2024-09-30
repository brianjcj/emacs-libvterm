;;; vterm-color.el --- colors for vterm -*- lexical-binding: t; -*-

;;; Commentary:

;; copy from ansi-color.el

;; just save the standard ansi color definitions to avoid overrieding by
;; some color themes!

;; Some color themes may mess up the term color. To use the clean standard ansi
;; colors in vterm:

;; (vterm-use-ansi-colors)

;; Back to the old one:
                                        ;
;; (vterm-use-vterm-colors)

;; We can also define more nice looking term color theme by simpling replacing
;; the vterm-color-palette-current.

;;; Code:

(defface vterm-ansi-color-black
  '((t :foreground "black" :background "black"))
  "Face used to render black color code."
  :group 'vterm)

(defface vterm-ansi-color-red
  '((t :foreground "red3" :background "red3"))
  "Face used to render red color code."
  :group 'vterm)

(defface vterm-ansi-color-green
  '((t :foreground "green3" :background "green3"))
  "Face used to render green color code."
  :group 'vterm)

(defface vterm-ansi-color-yellow
  '((t :foreground "yellow3" :background "yellow3"))
  "Face used to render yellow color code."
  :group 'vterm)

(defface vterm-ansi-color-blue
  '((t :foreground "blue2" :background "blue2"))
  "Face used to render blue color code."
  :group 'vterm)

(defface vterm-ansi-color-magenta
  '((t :foreground "magenta3" :background "magenta3"))
  "Face used to render magenta color code."
  :group 'vterm)

(defface vterm-ansi-color-cyan
  '((t :foreground "cyan3" :background "cyan3"))
  "Face used to render cyan color code."
  :group 'vterm)

(defface vterm-ansi-color-white
  '((t :foreground "grey90" :background "gray90"))
  "Face used to render white color code."
  :group 'vterm)

(defface vterm-ansi-color-bright-black
  '((t :foreground "gray30" :background "gray30"))
  "Face used to render bright black color code."
  :group 'vterm)

(defface vterm-ansi-color-bright-red
  '((t :foreground "red2" :background "red2"))
  "Face used to render bright red color code."
  :group 'vterm)

(defface vterm-ansi-color-bright-green
  '((t :foreground "green2" :background "green2"))
  "Face used to render bright green color code."
  :group 'vterm)

(defface vterm-ansi-color-bright-yellow
  '((t :foreground "yellow2" :background "yellow2"))
  "Face used to render bright yellow color code."
  :group 'vterm)

(defface vterm-ansi-color-bright-blue
  '((t :foreground "blue1" :background "blue1"))
  "Face used to render bright blue color code."
  :group 'vterm)

(defface vterm-ansi-color-bright-magenta
  '((t :foreground "magenta2" :background "magenta2"))
  "Face used to render bright magenta color code."
  :group 'vterm)

(defface vterm-ansi-color-bright-cyan
  '((t :foreground "cyan2" :background "cyan2"))
  "Face used to render bright cyan color code."
  :group 'vterm)

(defface vterm-ansi-color-bright-white
  '((t :foreground "white" :background "white"))
  "Face used to render bright white color code."
  :group 'vterm)


(defvar vterm-ansi-color-normal-colors-vector
  [vterm-ansi-color-black
   vterm-ansi-color-red
   vterm-ansi-color-green
   vterm-ansi-color-yellow
   vterm-ansi-color-blue
   vterm-ansi-color-magenta
   vterm-ansi-color-cyan
   vterm-ansi-color-white]
  "Faces used for SGR control sequences determining a color.
This vector holds the faces used for SGR control sequence parameters
30 to 37 (foreground colors) and 40 to 47 (background colors).

Parameter  Color
  30  40   black
  31  41   red
  32  42   green
  33  43   yellow
  34  44   blue
  35  45   magenta
  36  46   cyan
  37  47   white")

(defvar vterm-ansi-color-bright-colors-vector
  [vterm-ansi-color-bright-black
   vterm-ansi-color-bright-red
   vterm-ansi-color-bright-green
   vterm-ansi-color-bright-yellow
   vterm-ansi-color-bright-blue
   vterm-ansi-color-bright-magenta
   vterm-ansi-color-bright-cyan
   vterm-ansi-color-bright-white]
  "Faces used for SGR control sequences determining a \"bright\" color.
This vector holds the faces used for SGR control sequence parameters
90 to 97 (bright foreground colors) and 100 to 107 (bright background
colors).

Parameter   Color
  90  100   bright black
  91  101   bright red
  92  102   bright green
  93  103   bright yellow
  94  104   bright blue
  95  105   bright magenta
  96  106   bright cyan
  97  107   bright white")


(defvar vterm-ansi-color-colors-vector
  (vconcat vterm-ansi-color-normal-colors-vector
           vterm-ansi-color-bright-colors-vector))

(defun vterm-use-ansi-colors ()
  ""
  (interactive)
  (setq vterm-color-palette-current vterm-ansi-color-colors-vector))

(defun vterm-use-vterm-colors ()
  ""
  (interactive)
  (setq vterm-color-palette-current vterm-color-palette))

(provide 'vterm-color)

;;; vterm-color.el ends here
