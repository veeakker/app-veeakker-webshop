(define-resource account ()
  :class (s-prefix "foaf:OnlineAccount")
  :properties `((:email :string ,(s-prefix "account:email")))
  :resource-base (s-url "http://mu.semte.ch/vocabularies/accounts/")
  :on-path "accounts")
