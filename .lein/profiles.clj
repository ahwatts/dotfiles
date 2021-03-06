;; User profiles.
#_{:user
 {:aliases {"slamhound" ["run" "-m" "slam.hound"]}

  :dependencies [[slamhound "1.5.5"]
                 [org.clojure/tools.nrepl "0.2.12"]]

  :plugins [[lein-ancient "0.6.7" :exclusions [org.clojure/clojure org.clojure/data.xml]]
            [cider/cider-nrepl "0.16.0"]
            [lein-pprint "1.1.1"]]

  :jvm-opts
  #=(eval (vec (filter (complement nil?)
                       [(when (= "Mac OS X" (System/getProperty "os.name"))
                          "-Dapple.awt.UIElement=true")
                        (when (= "Windows 7" (System/getProperty "os.name"))
                          "-Dline.separator=\"\n\"")])))}}
{}
