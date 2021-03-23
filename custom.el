(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" default))
 '(package-selected-packages '(citeproc-org))
 '(safe-local-variable-values
   '((cider-shadow-default-options . ":test")
     (cider-preferred-build-tool . shadow-cljs)
     (cider-default-cljs-repl . shadow)
     (cider-shadow-cljs-default-options . "app")
     (cider-print-fn quote pr)
     (cider-print-fn . pr)
     (cider-default-cljs-repl . custom)
     (cider-figwheel-main-default-options . ":dev")
     (cider-default-cljs-repl . figwheel-main)
     (cider-clojure-cli-global-options . "-A:fig"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'customize-group 'disabled nil)

(define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
  (let ((obsolete-name (pop ll))
        (current-name (pop ll))
        (when (if ll (pop ll) "1"))
        (docstring (if ll (pop ll) nil)))
    (list obsolete-name current-name when docstring)))
