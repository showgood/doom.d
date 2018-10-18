;;; ~/.doom.d/+cc.el -*- lexical-binding: t; -*-


(defun ccls/base () (interactive) (lsp-ui-peek-find-custom 'base "$ccls/base"))
(defun ccls/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$ccls/callers"))
(defun ccls/vars (kind) (lsp-ui-peek-find-custom 'vars "$ccls/vars" (plist-put (lsp--text-document-position-params) :kind kind)))
(defun ccls/bases ()
  (interactive)
  (lsp-ui-peek-find-custom 'base "$ccls/inheritanceHierarchy"
                           (append (lsp--text-document-position-params) '(:flat t :level 3))))
(defun ccls/derived ()
  (interactive)
  (lsp-ui-peek-find-custom 'derived "$ccls/inheritanceHierarchy"
                           (append (lsp--text-document-position-params) '(:flat t :level 3 :derived t))))
(defun ccls/members ()
  (interactive)
  (lsp-ui-peek-find-custom 'base "$ccls/memberHierarchy"
                           (append (lsp--text-document-position-params) '(:flat t))))

;; The meaning of :role corresponds to https://github.com/maskray/ccls/blob/master/src/symbol.h

;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
(defun ccls/references-address ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 128))))

;; References w/ Role::Dynamic bit (macro expansions)
(defun ccls/references-macro ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 64))))

;; References w/o Role::Call bit (e.g. where functions are taken addresses)
(defun ccls/references-not-call ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:excludeRole 32))))

;; References w/ Role::Read
(defun ccls/references-read ()
  (interactive)
  (lsp-ui-peek-find-custom
   'read "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 8))))

;; References w/ Role::Write
(defun ccls/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom
   'write "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 16))))

(defun my/highlight-pattern-in-text (pattern line)
  (when (> (length pattern) 0)
    (let ((i 0))
      (while (string-match pattern line i)
        (setq i (match-end 0))
        (add-face-text-property (match-beginning 0) (match-end 0) 'isearch t line)
        )
      line)))

(def-package! clang-format
  :config
  (setq clang-format-style-option "llvm")
)

(defun +ccls//enable ()
  (condition-case nil
      (lsp-ccls-enable)
    (user-error nil)))

(def-package! ccls
  :after cc-mode
  :init (add-hook! (c-mode c++-mode objc-mode) #'+ccls//enable)
  :config
  ;; overlay is slow
  ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
  (setq ccls-sem-highlight-method 'font-lock)
  (ccls-use-default-rainbow-sem-highlight)
  ;; https://github.com/maskray/ccls/blob/master/src/config.h
  (setq ccls-extra-init-params
        '(:completion
          (
           :detailedLabel t
           :includeBlacklist
           ("^/usr/(local/)?include/c\\+\\+/[0-9\\.]+/(bits|tr1|tr2|profile|ext|debug)/"
            "^/usr/(local/)?include/c\\+\\+/v1/"
            ))
          :diagnostics (:frequencyMs 5000)
          :index (:reparseForDependency 1)))

  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

  (evil-set-initial-state 'ccls-tree-mode 'emacs)
  (set! :company-backend '(c-mode c++-mode objc-mode) 'company-lsp)
)
