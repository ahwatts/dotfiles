(deftask cider "CIDER profile"
  []
  (require 'boot.repl)
  (swap! @(resolve 'boot.repl/*default-dependencies*)
         concat '[[org.clojure/tools.nrepl "0.2.11"]
                  [cider/cider-nrepl "0.12.0"]])
  (swap! @(resolve 'boot.repl/*default-middleware*)
         concat '[cider.nrepl/cider-middleware])
  identity)
