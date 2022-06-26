;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Harrison Fretwell"
      user-mail-address "hey@harrisonfretwell.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "MonoLisa Nerd Font" :size 14 :weight 'normal)
      doom-variable-pitch-font (font-spec :family "Alegreya SC" :size 15)
      )
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Autosave and backup
(setq auto-save-default t
      make-backup-files t)
;;Disable exit warning
(setq confirm-kill-emacs nil)
;;Maximise window on startup
;;(add-to-list 'initial-frame-alist '(maximized))
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))


;; Allow hash to be entered
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; accept completion from copilot and fallback to company
;; (defun my-tab ()
;;   (interactive)
;;   (or (copilot-accept-completion)
;;       (company-indent-or-complete-common nil)))

;; (use-package! copilot
;;   :hook (prog-mode . copilot-mode)
;;   :bind (("C-TAB" . 'copilot-accept-completion-by-word)
;;          ("C-<tab>" . 'copilot-accept-completion-by-word)
;;          :map company-active-map
;;          ("<tab>" . 'my-tab)
;;          ("TAB" . 'my-tab)
;;          :map company-mode-map
;;          ("<tab>" . 'my-tab)
;;          ("TAB" . 'my-tab)))

(with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'typescript-mode)
    (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
)
(setq-hook! 'js2-mode-hook flycheck-checker 'javascript-eslint)
(setq-hook! 'typescript-mode-hook flycheck-checker 'javascript-eslint)
(setq-hook! 'typescript-tsx-mode-hook flycheck-checker 'javascript-eslint)

;; Setup prettier
(add-hook 'after-init-hook #'global-prettier-mode)
(after! prettier
        (add-hook 'typescript-mode-hook 'prettier-mode)
        (add-hook 'js2-mode-hook 'prettier-mode)
        (add-hook 'typescript-tsx-mode-hook 'prettier-mode)
        )


(evil-snipe-override-mode 1)
(use-package! evil-snipe
  :config (setq evil-snipe-scope 'buffer
                evil-snipe-repeat-scope 'visible
                evil-snipe-enable-highlight t
                evil-snipe-enable-incremental-highlight t
                evil-snipe-enable-key-bindings t))

;; Disable certain ligatures
(plist-put! +ligatures-extra-symbols
  :true          nil
  :false         nil
)
(let ((ligatures-to-disable '(:true :false)))
  (dolist (sym ligatures-to-disable)
    (plist-put! +ligatures-extra-symbols sym )))

;; Set ligatures for typescript mode
(after! typescript-tsx-mode
  (set-ligatures! 'typescript-tsx-mode
        ;; Functional
        :def "function"
        :lambda "() =>"
        :composition "compose"
        ;; Types
        :null "null"
        :true "true" :false "false"
        ;; Flow
        :not "!"
        :and "&&" :or "||"
        :for "for"
        :return "return"
        ;; Other
        :yield "import"
        )
)

;; Org journal
(setq org-journal-date-prefix "#+TITLE: "
      org-journal-time-prefix "* "
      org-journal-date-format "%a, %Y-%m-%d"
      org-journal-file-format "%Y-%m-%d.org")


;;GPG path setup
(setq auth-sources '("~/.authinfo.gpg"))
(require 'epa-file)
(custom-set-variables '(epg-gpg-program  "/opt/homebrew/opt/gnupg@2.2/bin/gpg"))
(epa-file-enable)


;;Better tsx
;;(require 'tsi)
;;(require 'tsx-mode)
