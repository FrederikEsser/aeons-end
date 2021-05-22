(ns aeons-end.cards.nemesis)

(def unleash-1 {:name        :unleash-1
                :text        "Unleash."
                :type        :attack
                :immediately [[:unleash]]
                :tier        1})

(def unleash-2 {:name        :unleash-2
                :text        "Unleash twice."
                :type        :attack
                :immediately [[:unleash]
                              [:unleash]]
                :tier        2})

(def unleash-3 {:name        :unleash-3
                :text        "Unleash three times."
                :type        :attack
                :immediately [[:unleash]
                              [:unleash]
                              [:unleash]]
                :tier        3})

(def cards (mapcat vector
                   (repeat unleash-1)
                   (repeat unleash-2)
                   (repeat unleash-3)))
