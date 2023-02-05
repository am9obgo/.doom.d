;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;**************************************************************************************;;
;; GENERAL SETTINGS
;;**************************************************************************************;;

;; USER NAME
(setq user-full-name "am9obgo" user-mail-address "-")

;; LINE NUMBERS
(setq display-line-numbers-type nil)

;; FRAME TITLE
(setq frame-title-format "%f")

;; COLUMN WIDTH
(setq-default fill-column 90)

;; TAB WIDTH
(setq-default tab-width 4)

;; INDENT TABS
(setq-default indent-tabs-mode nil)

;; SCROLL UP/DOWN
(setq scroll-error-top-bottom t)

;;**************************************************************************************;;
;; BUILT-IN MODES
;;**************************************************************************************;;

;;
;; UNIQUIFY
;; Overrides Emacs’ default mechanism for making buffer names unique
;;
;; https://www.emacswiki.org/emacs/uniquify
;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;;
;; WINNER-MODE
;;
;; Allows to “undo” (and “redo”) changes in the window configuration with the key
;; commands ‘C-c left’ and ‘C-c right’.
;; https://www.emacswiki.org/emacs/WinnerMode
;;

(require 'winner)
(winner-mode t)

;;
;; PAREN MODE
;;
;; Allows one to see matching pairs of parentheses and other characters.
;; https://www.emacswiki.org/emacs/ShowParenMode
;;

(show-paren-mode t)

;;
;; IBUFFER
;;
;; Advanced replacement for BufferMenu, which lets you operate on buffers much in the same
;; manner as Dired.
;; https://www.emacswiki.org/emacs/IbufferMode
;;

(setq ibuffer-formats
      '((mark modified read-only locked " "
              (icon 2 2 :left :elide)
              #(" " 0 1
                (display
                 (space :align-to 8)))
              (name 32 32 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

;;
;; CC-MODE
;;
;; CC Mode is a GNU Emacs mode for editing files containing C, C++, Objective-C, Java,
;; CORBA IDL (and the variants PSDL and CIDL), Pike and AWK code.
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
;;

(add-hook 'c-mode-common-hook
          (lambda()
            (fset 'c-indent-region 'clang-format-region)
            ;; (setq c-basic-offset 4)
            ;; (c-set-offset 'access-label '--)
            ;; (c-set-offset 'arglist-intro '++)
            ;; (c-set-offset 'class-open '-)
            ;; (c-set-offset 'innamespace 0)
            ;; (c-set-offset 'substatement-open 0)
            ;; (c-set-offset 'topmost-intro '-)
            ))

;;
;; IVY
;;
;; Ivy is an interactive interface for completion in Emacs. Emacs uses completion
;; mechanism in a variety of contexts: code, menus, commands, variables, functions, etc.
;; https://github.com/abo-abo/swiper
;;

(setq ivy-extra-directories nil)

;;
;; PROJECTILE
;;
;; Projectile is a project interaction library for Emacs. It provides a nice set of
;; features operating on a project level without introducing external dependencies.
;; https://github.com/bbatsov/projectile
;;

(after! projectile
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml"))

;;
;; DAP
;;
;; Emacs client/library for Debug Adapter Protocol is a wire protocol for communication
;; between client and Debug Server. It's similar to the LSP but provides integration with
;; debug server.
;;

(after! dap-mode
  (setq dap-python-debugger 'debugpy)
  (setq dap-auto-configure-mode t)
  (require 'dap-cpptools))

;;
;; CMAKE MODE
;;
;;

(add-to-list 'auto-mode-alist '("CMakeLists\\.var\\'" . cmake-mode))

;;**************************************************************************************;;
;; CUSTOM ROUTINES
;;**************************************************************************************;;

;; SELECT WHOLE WORD
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))
(defun extend-selection (arg &optional incremental)
  "Select the current word."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

;; SELECT TEXT IN QUOTE
(defun select-text-in-quote ()
  "Select text between the nearest left and right delimiters."
  (interactive)
  (let (b1)
    (skip-chars-backward "^<>(“{[「«\"‘")
    (setq b1 (point))
    (skip-chars-forward "^<>)”}]」»\"’")
    (set-mark b1)))

;; OSX PASTEBIN INTERFACE
(setq last-paste-to-osx nil)
(defun copy-from-osx ()
  (let ((copied-text (shell-command-to-string "pbpaste -Prefer txt")))
    (unless (string= copied-text last-paste-to-osx) copied-text)))
(defun paste-to-osx (text &optional _)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc)))
  (setq last-paste-to-osx text))

;;*************************************************************************************;;
;; OS SPECIFIC SETTINGS
;;*************************************************************************************;;

;; THEME
(cond
 ;; Linux
 ((string= system-type "gnu/linux")
  (setq doom-theme 'doom-monokai-spectrum))
 ;; macOS
 ((string= system-type "darwin")
    (if (string= "true"
                 (shell-command-to-string
                  (concat "printf %s \"$(osascript -e "
                          "\'tell application \"System Events\" to "
                          "tell appearance preferences to return dark mode\')\"")))
        (setq doom-theme 'doom-one)
      (setq doom-theme 'doom-one-light)))
 ;; Windows ;)
 (setq doom-theme 'doom-one-ligth))

;; VISUAL BEHAVIOR
(cond
 ((string= system-type "darwin")
  (when (not window-system)
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx))
  (setq doom-font "SF Mono-12")
  (setq explicit-bash-args '("--noediting" "--login"))
  (add-to-list 'default-frame-alist (cons 'width 140))
  (add-to-list 'default-frame-alist (cons 'height 60))))

;;*************************************************************************************;;
;; KEYBOARD BINDINGS
;;*************************************************************************************;;

(map! "M-*" 'select-text-in-quote
      "M-#" 'extend-selection
      "C-s" 'swiper-isearch)

(map! "ESC <up>" 'windmove-up
      "ESC <down>" 'windmove-down
      "ESC <left>" 'windmove-left
      "ESC <right>" 'windmove-right)

(map! :map dap-mode-map
      "<f5>" 'dap-debug
      "C-<f5>" 'dap-continue
      "S-<f5>" 'dap-delete-all-sessions
      "<f9>" 'dap-breakpoint-toggle
      "<f10>" 'dap-next
      "S-<f11>" 'dap-step-out
      "<f11>" 'dap-step-in)

(map! :map smartparens-mode-map
      "C-M-a" 'sp-beginning-of-sexp
      "C-M-e" 'sp-end-of-sexp
      "C-M-d" 'sp-down-sexp
      "C-M-u" 'sp-up-sexp
      "M-<down>" 'sp-backward-down-sexp
      "M-<up>" 'sp-backward-up-sexp
      "C-M-f" 'sp-forward-sexp
      "C-M-b" 'sp-backward-sexp
      "C-M-n" 'sp-next-sexp
      "C-M-p" 'sp-previous-sexp
      "C-M-t" 'sp-transpose-sexp
      "C-M-k" 'sp-kill-sexp-with-a-twist-of-lime
      "C-M-w" 'sp-copy-sexp
      "M-s" 'sp-splice-sexp
      "M-r" 'sp-splice-sexp-killing-around
      "C-)" 'sp-forward-slurp-sexp
      "C-}" 'sp-forward-barf-sexp
      "C-(" 'sp-backward-slurp-sexp
      "C-{" 'sp-backward-barf-sexp
      "M-S" 'sp-split-sexp
      "M-J" 'sp-join-sexp
      "C-<tab>" 'sp-indent-defun)

(map! :map ivy-minibuffer-map
      "TAB" 'ivy-partial-or-done)
