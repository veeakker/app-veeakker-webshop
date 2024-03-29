(in-package :mu-cl-resources)

(define-resource account ()
  :class (s-prefix "foaf:OnlineAccount")
  :properties `((:email :string ,(s-prefix "account:email")))
  :has-one `((person :via ,(s-prefix "foaf:account")
                     :inverse t
                     :as "person"))

  :resource-base (s-url "http://mu.semte.ch/vocabularies/accounts/")
  :on-path "accounts")

(define-resource person ()
  :class (s-prefix "foaf:Person")
  :properties `((:first-name :string ,(s-prefix "foaf:firstName"))
                (:last-name :string ,(s-prefix "foaf:familyName")))
  :has-one `((postal-address :via ,(s-prefix "schema:PostalAddress")
                             :as "postal-address"))
  :has-many `((account :via ,(s-prefix "foaf:account")
                       :as "accounts"))
  :resource-base (s-url "http://veeakker.be/people/")
  :on-path "people")
