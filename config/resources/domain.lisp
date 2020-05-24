(in-package :mu-cl-resources)

(setf *verify-content-type-header* nil)
(setf *verify-accept-header* nil)

(define-resource organization ()
  :class (s-prefix "schema:Organization")
  :has-many `((delivery-place :via ,(s-prefix "schema:hasPos")
                              :as "delivery-places"))
  :resource-base (s-url "http://veeakker.be/organizations/")
  :on-path "organizations")

(define-resource delivery-place ()
  :class (s-prefix "veeakker:DeliveryPlace")
  :has-one `((delivery-kind :via ,(s-prefix "veeakker:hasDeliveryKind")
                            :as "delivery-kind")
             (geo-coordinate :via ,(s-prefix "schema:geo")
                             :as "geo-coordinate")
             (postal-address :via ,(s-prefix "schema:hasAddress")
                             :as "postal-address"))
  :resource-base (s-url "http://veeakker.be/delivery-places/")
  :on-path "delivery-places")

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
  :has-one `((product-group :via ,(s-prefix "skos:broader")
                            :as "parent-group"))
  :has-many `((product-group :via ,(s-prefix "skos:broader")
                             :inverse t
                             :as "child-groups")
              (product :via ,(s-prefix "veeakker:hasProduct")
                       :as "products")
              (spotlight-product :via ,(s-prefix "veeakker:hasSpotlight")
                                 :as "spotlight-products"))
  :resource-base (s-url "http://veeakker.be/product-groups/")
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
                (:description :string ,(s-prefix "dct:description")))
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
  :has-one `((type-and-quantity :via ,(s-prefix "gr:includesObject")
                                :as "type-and-quantity")
             (unit-price-specification :via ,(s-prefix "gr:hasPriceSpecification")
                                       :as "unit-price"))
  :resource-base (s-url "http://veeakker.be/offerings/")
  :on-path "offerings")


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
                (:status-changed-at :datetime ,(s-prefix "veeakker:statusChangedAt")))
  :has-many `((order-line :via ,(s-prefix "veeakker:orderLine")
                          :as "order-lines"))
  :resource-base (s-url "http://veeakker.be/baskets/")
  :on-path "baskets")

(define-resource order-line ()
  :class (s-prefix "veeakker:OrderLine")
  :properties `((:amount :number ,(s-prefix "veeakker:amount")))
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
