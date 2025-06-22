(in-package :mu-cl-resources)

(setf *verify-content-type-header* nil)
(setf *verify-accept-header* nil)
(setf *include-count-in-paginated-responses* t)
(setf *supply-cache-headers-p* t)
(setf sparql:*experimental-no-application-graph-for-sudo-select-queries* t)
(setf *cache-model-properties-p* t)
(setf mu-support::*use-custom-boolean-type-p* nil)
(setf *backtrace-on-call-error-types* (list 'error))
(setf sparql:*query-log-types* nil)  ;; '(:default :update-group :update :query :ask)

(define-resource banner ()
  :class (s-prefix "ext:Banner")
  :properties `((:title :string ,(s-prefix "dct:title"))
                (:kind :url ,(s-prefix "ext:kind"))
                (:is-enabled :boolean ,(s-prefix "veeakker:isEnabled"))
                (:sort-index :number ,(s-prefix "veeakker:sortIndex")))
  :resource-base (s-url "http://veeakker.be/banners/")
  :on-path "banners")

(define-resource organization ()
  :class (s-prefix "schema:Organization")
  :has-many `((delivery-place :via ,(s-prefix "schema:hasPos")
                              :as "delivery-places"))
  :resource-base (s-url "http://veeakker.be/organizations/")
  :on-path "organizations")

(define-resource delivery-place ()
  :class (s-prefix "veeakker:DeliveryPlace")
  :properties `((:is-enabled :boolean ,(s-prefix "veeakker:isEnabled"))
                (:label  :string ,(s-prefix "dct:title"))
                (:slug :string ,(s-prefix "ext:slug"))
                (:lfw-link :url ,(s-prefix "ext:lfw-link")))
  :has-one `((delivery-kind :via ,(s-prefix "veeakker:hasDeliveryKind")
                            :as "delivery-kind")
             (geo-coordinate :via ,(s-prefix "schema:geo")
                             :as "geo-coordinate")
             (postal-address :via ,(s-prefix "schema:hasAddress")
                             :as "postal-address")
             (delivery-route :via ,(s-prefix "veeakker:belongsToRoute")
                             :as "delivery-route"))
  :resource-base (s-url "http://veeakker.be/delivery-places/")
  :on-path "delivery-places")

(define-resource delivery-route ()
  :class (s-prefix "veeakker:DeliveryRoute")
  :properties `((:label :string ,(s-prefix "dct:title"))
                (:next-delivery-date :date ,(s-prefix "veeakker:deliveryDate"))
                (:lfw-link :url ,(s-prefix "ext:lfw-link")))
  :has-many `((delivery-place :via ,(s-prefix "veeakker:belongsToRoute")
                              :inverse t
                              :as "delivery-places"))
  :resource-base (s-url "http://veeakker.be/delivery-routes/")
  :on-path "delivery-routes")

(define-resource delivery-kind ()
  :class (s-prefix "veeakker:DeliveryKind")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:description :string ,(s-prefix "dct:description")))
  :has-many `((delivery-place :via ,(s-prefix "veeakker:hasDeliveryKind")
                              :inverse t
                              :as "delivery-places"))
  :resource-base (s-url "http://veeakker.be/delivery-kinds/")
  :features '(include-uri)
  :on-path "delivery-kinds")

(define-resource geo-coordinate ()
  :class (s-prefix "schema:GeoCoordinate")
  :properties `((:latitude :number ,(s-prefix "schema:latitude"))
                (:longitude :number ,(s-prefix "schema:longitude")))
  :has-one `((postal-address :via ,(s-prefix "schema:address")
                             :as "postal-address"))
  :resource-base (s-url "http://veeakker.be/geo-coordinates/")
  :on-path "geo-coordinates")

(define-resource postal-address ()
  :class (s-prefix "schema:PostalAddress")
  :properties `((:country :string ,(s-prefix "schema:addressCountry"))
                (:locality :string ,(s-prefix "schema:addressLocality"))
                (:postal-code :string ,(s-prefix "schema:postalCode"))
                (:street-address :string ,(s-prefix "schema:streetAddress")))
  :resource-base (s-url "http://veeakker.be/postal-addresses/")
  :on-path "postal-addresses")

(define-resource product-group ()
  :class (s-prefix "veeakker:ProductGroup")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:sort-index :number ,(s-prefix "veeakker:sortIndex")))
  :has-many `((product-group :via ,(s-prefix "skos:broader")
                             :inverse t
                             :as "child-groups")
              (product-group :via ,(s-prefix "skos:broader")
                             :as "parent-groups")
              (product :via ,(s-prefix "veeakker:hasProduct")
                       :as "products")
              (spotlight-product :via ,(s-prefix "veeakker:hasSpotlight")
                                 :as "spotlight-products"))
  :resource-base (s-url "http://veeakker.be/product-groups/")
  :features '(include-uri)
  :on-path "product-groups")


;; Working with products
;;
;; - The product is the thing that used to have a PLU.
;; -> It has a unit price
;; -> It has an image
;; -> It has a description
;; -> + any other metadata needed to inform
;;
;; - The Offer is an offer for a single Product
;; -> It contains the price for that specific Offer
;; -> It contains the amount for that specific Offer

(define-resource product ()
  :class (s-prefix "schema:Product")
  :properties `((:label :string ,(s-prefix "dct:title"))
                (:alt-label :string ,(s-prefix "skos:altLabel"))
                (:sort-index :number ,(s-prefix "veeakker:sortIndex"))
                (:product-labels :uri-set ,(s-prefix "veeakker:hasLabel"))
                (:allergens-text :string ,(s-prefix "veeakker:allergensAsText"))
                (:nutricion-data-text :string ,(s-prefix "veeakker:nutricionDataAsText")) ; should be served by nutricion data in the future
                (:description :string ,(s-prefix "dct:description"))
                (:ingredients-text :string ,(s-prefix "food:ingredientListAsText"))
                (:plu :number ,(s-prefix "veeakker:plu"))
                (:is-enabled :boolean ,(s-prefix "veeakker:isPublic")))
  :has-many `((product-group :via ,(s-prefix "veeakker:hasProduct")
                             :inverse t
                             :as "product-groups")
              (offering :via ,(s-prefix "veeakker:offerings")
                        :as "offerings"))
  :has-one `((unit-price-specification :via ,(s-prefix "veeakker:singleUnitPrice")
                                       :as "unit-price")
             (quantitative-value :via ,(s-prefix "veeakker:targetUnit")
                                 :as "target-unit")
             (file :via ,(s-prefix "veeakker:thumbnail")
                   :as "thumbnail"))
  :resource-base (s-url "http://veeakker.be/products/")
  :on-path "products")

(define-resource offering ()
  :class (s-prefix "gr:Offering")
  :properties `((:is-enabled :boolean ,(s-prefix "veeakker:isEnabled")))
  :has-one `((type-and-quantity :via ,(s-prefix "gr:includesObject")
                                :as "type-and-quantity")
             (unit-price-specification :via ,(s-prefix "gr:hasPriceSpecification")
                                       :as "unit-price")
             (business-entity :via ,(s-prefix "gr:offers")
                              :inverse t
                              :as "supplier"))
  :resource-base (s-url "http://veeakker.be/offerings/")
  :on-path "offerings")

(define-resource business-entity ()
  :class (s-prefix "gr:BusinessEntity")
  :properties `((:name :string ,(s-prefix "gr:name"))
                (:email :string ,(s-prefix "schema:email"))
                ;; description contains HTML
                (:description :string ,(s-prefix "dct:description")))
  :resource-base (s-url "http://veeakker.be/business-entities/")
  :on-path "business-entities")

(define-resource file ()
  :class (s-prefix "nfo:FileDataObject")
  :properties `((:filename :string ,(s-prefix "nfo:fileName"))
                (:format :string ,(s-prefix "dct:format"))
                (:size :number ,(s-prefix "nfo:fileSize"))
                (:extension :string ,(s-prefix "dbpedia:fileExtension"))
                (:created :datetime ,(s-prefix "nfo:fileCreated")))
  :has-one `((file :via ,(s-prefix "nie:dataSource")
                   :inverse t
                   :as "download"))
  :resource-base (s-url "http://veeakker.be/files/")
  :features `(include-uri)
  :on-path "files")

(define-resource favourite ()
  :class (s-prefix "ext:Favourite")
  :properties `((:created :datetime ,(s-prefix "nfo:fileCreated")))
  :has-one `((product :via ,(s-prefix "schema:Product")
                      :as "product")
             (person :via ,(s-prefix "foaf:Person")
                     :inverse t
                     :as "person"))
                                       
  :resource-base (s-url "http://veeakker.be/favourites/")
  :on-path "favourites")

;; General support
(define-resource unit-price-specification ()
  :class (s-prefix "gr:UnitPriceSpecification")
  :properties `((:unit :string ,(s-prefix "gr:hasUnitOfMeasurement"))
                (:value :number ,(s-prefix "gr:hasCurrencyValue")))
  :resource-base (s-url "http://veeakker.be/price-specifications/")
  :on-path "unit-price-specifications")

(define-resource quantitative-value ()
  :class (s-prefix "gr:QuantitativeValue")
  :properties `((:unit :string ,(s-prefix "gr:hasUnitOfMeasurement"))
                (:value :number ,(s-prefix "gr:hasValue")))
  :resource-base (s-url "http://veeakker.be/quantitative-values/")
  :on-path "quantitative-values")

(define-resource type-and-quantity ()
  :class (s-prefix "gr:TypeAndQuantityNode")
  :properties `((:value :number ,(s-prefix "gr:amountOfThisGood"))
                (:unit :string ,(s-prefix "gr:hasUnitOfMeasurement")))
  :has-one `((product :via ,(s-prefix "gr:typeOfGood")
                      :as "product"))
  :resource-base (s-url "http://veeakker.be/type-and-quantities/")
  :on-path "type-and-quantities")

(define-resource basket ()
  :class (s-prefix "veeakker:Basket")
  :properties `((:payment-status :string ,(s-prefix "veeakker:basketPaymentStatus"))
                (:order-status :url ,(s-prefix "veeakker:basketOrderStatus"))
                (:status-changed-at :datetime ,(s-prefix "veeakker:statusChangedAt"))
                (:has-custom-delivery-place :boolean ,(s-prefix "veeakker:hasCustomDeliveryPlace"))
                (:delivery-type :url ,(s-prefix "veeakker:deliveryType")))
  :has-many `((order-line :via ,(s-prefix "veeakker:orderLine")
                          :as "order-lines"))
  :has-one `((delivery-place :via ,(s-prefix "veeakker:deliveryPlace")
                             :as "delivery-place")
             (full-address :via ,(s-prefix "veeakker:invoiceAddress")
                           :as "invoice-address")
             (full-address :via ,(s-prefix "veeakker:deliveryAddress")
                           :as "delivery-address")
             (person :via ,(s-prefix "veeakker:orderedBy")
                     :as "customer"))
  :resource-base (s-url "http://veeakker.be/baskets/")
  :on-path "baskets")

(define-resource full-address ()
  :class (s-prefix "veeakker:Address")
  :properties `((:name :string ,(s-prefix "foaf:name"))
                (:company :string ,(s-prefix "ext:companyInfo"))
                (:telephone :string ,(s-prefix "foaf:phone"))
                (:email :string ,(s-prefix "schema:email"))) ; used for basket ordering process
  :has-one `((postal-address :via ,(s-prefix "schema:hasAddress")
                             :as "postal-address"))
  :resource-base (s-url "http://veeakker.be/full-addresses/")
  :on-path "full-addresses")

(define-resource order-line ()
  :class (s-prefix "veeakker:OrderLine")
  :properties `((:amount :number ,(s-prefix "veeakker:amount"))
                (:comment :string ,(s-prefix "veeakker:customerComment")))
  :has-one `((offering :via ,(s-prefix "veeakker:hasOffering")
                       :as "offering"))
  :resource-base (s-url "http://mu.semte.ch/order-lines/")
  :on-path "order-lines")

(define-resource spotlight-product ()
  :class (s-prefix "veeakker:SpotlightProduct")
  :properties `((:sort-index :number ,(s-prefix "veeakker:sortIndex")))
  :has-one `((product :via ,(s-prefix "veeakker:promotesProduct")
                      :as "product"))
  :has-many `((product-group :via ,(s-prefix "veeakker:hasSpotlight")
                             :inverse t
                             :as "product-groups"))
  :resource-base (s-url "https://veeakker.be/spotlight-products/")
  :on-path "spotlight-products")

(define-resource file ()
  :class (s-prefix "nfo:FileDataObject")
  :properties `((:filename :string ,(s-prefix "nfo:fileName"))
                (:format :string ,(s-prefix "dct:format"))
                (:size :number ,(s-prefix "nfo:fileSize"))
                (:extension :string ,(s-prefix "dbpedia:fileExtension"))
                (:created :datetime ,(s-prefix "nfo:fileCreated")))
  :has-one `((file :via ,(s-prefix "nie:dataSource")
                   :inverse t
                   :as "download"))
  :resource-base (s-url "http://veeakker.be/files/")
  :features `(include-uri)
  :on-path "files")

(define-resource person ()
  :class (s-prefix "foaf:Person")
  :properties `((:first-name :string ,(s-prefix "foaf:firstName"))
                (:last-name :string ,(s-prefix "foaf:familyName"))
                (:email :string ,(s-prefix "schema:email"))
                (:phone :string ,(s-prefix "foaf:phone")))
  :has-one `((postal-address :via ,(s-prefix "schema:postalAddress")
                             :as "postal-address"))
  :has-many `((account :via ,(s-prefix "foaf:account")
                       :as "accounts")
              (favourite :via ,(s-prefix "ext:Favourite")
                         :as "favourites"))
  :resource-base (s-url "http://veeakker.be/people/")
  :on-path "people")

 (define-resource account ()
  :class (s-prefix "foaf:OnlineAccount")
  :properties `((:email :string ,(s-prefix "account:email")))
  :has-one `((person :via ,(s-prefix "foaf:account")
                     :inverse t
                     :as "person"))

  :resource-base (s-url "http://mu.semte.ch/vocabularies/accounts/")
  :on-path "accounts")

;; We would like to start using the following
;;
;;
;; frapo:hasCurrency
;; frapo:hasValue
;;
;; frapo:hasDeliveryDate
;;
;; frapo:hasCurrencyCode ?
;;
;; frapo:Quotation
;;   - frapo:hasQuotationDate
;; frapo:PurchaseOrder
;; frapo:Invoice
;;   - frapo:hasInvoiceDate
;; frapo:Payment
;; frapo:Purchase
;;   - frapo:hasPurchaseDate
;;
;;
;; veeakker:Hamburger a gr:ProdutOrServiceModel;
;;   gr:name "Hamburger"@nl.
;; veeakker:RundsBurger a gr:ProductOrServiceModel;
;;  gr:name "RundsBurger"@nl;
;;  gr:isVariantOf veeakker:Hamburger;
;;  veeakker:hasSpices [ ... ].
;; veeakker:RundsBurger2Stuks;
;;  gr:isVariantOf veeakker:RundsBurger;
;;  veeakker:numberOfItems [
;;    a gr:QuantitativeValue;
;;    gr:hasValue "2"^^xsd:int;
;;    gr:hasUnitOfMeasurement "C62"^^xsd:string.
;;  ].


;; frapo http://purl.org/cerif/frapo/

(defcall :delete (base-path id)
  (with-user-configurable-backtrace
    (with-single-itemspec-classes-retry
      (delete-call (find-resource-by-path base-path) id))))

(defun maybe-print-backtrace-for-toplevel-error (e)
  "Prints a stacktrace for error E if the user has requested the
system to do so."
  (when (some (lambda (type) (typep e type)) *backtrace-on-call-error-types*)
    (trivial-backtrace:print-backtrace e)))
