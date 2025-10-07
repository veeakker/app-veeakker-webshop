;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

;; (add-delta-logger)
(add-delta-messenger "http://delta-notifier/")

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* nil)
(setf *backend* "http://triplestore:8890/sparql")

(in-package :server)
(setf *log-incoming-requests-p* nil)


;;;;;;;;;;;;;;;;;
;;; access rights
(in-package :acl)

(define-prefixes
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :gr "http://purl.org/goodrelations/v1#"
  :nfo "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#"
  :schema "http://schema.org/"
  :service "http://services.semantic.works/"
  :veeakker "http://veeakker.be/vocabularies/shop/"
  :veeakkerservice "http://services.veeakker.be/"
  :skos "http://www.w3.org/2004/02/skos/core#"
  :foaf "http://xmlns.com/foaf/0.1/"
  :adms "http://www.w3.org/ns/adms#"
  :dct "http://purl.org/dc/terms/"
  :mu "http://mu.semte.ch/vocabularies/core/")

(define-graph public ("http://mu.semte.ch/graphs/public")
  ("schema:Organization" -> _ <- _)
  ("veeakker:DeliveryPlace"  -> _ <- _)
  ("veeakker:DeliveryKind" -> _ <- _)
  ("veeakker:DeliveryRoute" -> _)
  ("schema:GeoCoordinate" -> _ <- _)
  ("schema:PostalAddress" -> _ <- _)
  ("veeakker:ProductGroup" -> _ <- _)
  ("schema:Product" -> _ <- _)
  ("gr:Offering" -> _ <- _) ;; includes <- gr:offers
  ("gr:UnitPriceSpecification" -> _ <- _)
  ("gr:QuantitativeValue" -> _ <- _)
  ("gr:TypeAndQuantityNode" -> _ <- _)
  ("veeakker:SpotlightProduct" -> _ <- _)
  ("nfo:FileDataObject" -> _ <- _)
  ("ext:Banner" -> _ <- _)
  ("gr:BusinessEntity" -> "veeakker:hasDeliveryPlace"))

(define-graph external-identifiers ("http://mu.semte.ch/graphs/external-identifiers")
  ("adms:Identifier" -> _))

(define-graph lfw-import-jobs ("http://mu.semte.ch/graphs/lfw-import-jobs")
  ("veeakker:LfwFetchJob"
   -> "rdf:type"
   -> "mu:uuid"
   -> "dct:created"
   -> "adms:status"))

(define-graph lfw-extra-info ("http://mu.semte.ch/graphs/lfw-extra-info")
  ("gr:BusinessEntity"
   -> "rdf:type"
   -> "mu:uuid"
   -> "gr:name"
   -> "adms:identifier"
   -> "schema:email"
   -> "dct:description"))

(define-graph files ("http://mu.semte.ch/graphs/public")
  ("nfo:FileDataObject" -> _ <- _))

(supply-allowed-group "public")

(supply-allowed-group "admin"
  :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
          PREFIX veeakker: <http://veeakker.be/vocabularies/shop/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT ?id WHERE {
          <SESSION_ID> session:account/^foaf:account ?person.
          GRAPH <http://veeakker.be/graphs/administrators> {
            ?person veeakker:role veeakker:Administrator.
          }
         }"
  :parameters ())

(define-graph anonymous-session-graph ("http://mu.semte.ch/sessions/")
  (_ -> "veeakker:graphBelongsToSession")
  (_ -> "veeakker:hasBasket")
  ("veeakker:Basket"
   -> "rdf:type"
   -> "mu:uuid"
   -> "veeakker:orderedBy"
   -> "veeakker:changedAt"
   -> "veeakker:basketOrderStatus"
   -> "veeakker:statusChangedAt"
   -> "veeakker:deliveryAddress"
   -> "veeakker:invoiceAddress"
   -> "veeakker:deliveryType"
   -> "veeakker:deliveryPlace"
   -> "veeakker:orderLine"
   -> "veeakker:hasCustomDeliveryPlace")
  ("veeakker:OrderLine"
   -> "rdf:type"
   -> "mu:uuid"
   -> "veeakker:amount"
   -> "veeakker:customerComment"
   -> "veeakker:hasOffering")
  ("veeakker:Address"
   -> "rdf:type"
   -> "mu:uuid"
   -> "schema:hasAddress"
   -> "foaf:firstName"
   -> "foaf:lastName"
   -> "ext:companyInfo"
   -> "foaf:phone"
   -> "schema:email")
  ("schema:PostalAddress."
   -> "rdf:type"
   -> "mu:uuid"
   -> "schema:addressLocality"
   -> "schema:postalCode"
   -> "schema:streetAddress"))

(supply-allowed-group "anonymous-session"
  :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
          PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          SELECT ?id
          WHERE {
            VALUES ?graph { <SESSION_ID> }
            BIND ( strafter(str(?graph),\"/sessions/\") AS ?id )
            FILTER ( ?id != \"\" )
            FILTER NOT EXISTS {
              <SESSION_ID> session:account/^foaf:account ?user.
            }
          } ORDER BY ?id LIMIT 1"
  :parameters (list "id"))

(define-graph logged-in-graph ("http://veeakker.be/people/")
   (_ -> "veeakker:graphBelongsToUser")
   ("foaf:OnlineAccount" -> _)
   ("foaf:Person" -> _)
   (_ -> "veeakker:graphBelongsToSession")
   (_ -> "veeakker:hasBasket")
   ("veeakker:Basket"
    -> "rdf:type"
    -> "mu:uuid"
    -> "veeakker:orderedBy"
    -> "veeakker:changedAt"
    -> "veeakker:basketOrderStatus"
    -> "veeakker:statusChangedAt"
    -> "veeakker:deliveryAddress"
    -> "veeakker:invoiceAddress"
    -> "veeakker:deliveryType"
    -> "veeakker:deliveryPlace"
    -> "veeakker:orderLine"
    -> "veeakker:hasCustomDeliveryPlace")
   ("veeakker:OrderLine"
    -> "rdf:type"
    -> "mu:uuid"
    -> "veeakker:amount"
    -> "veeakker:customerComment"
    -> "veeakker:hasOffering")
   ("veeakker:Address"
    -> "rdf:type"
    -> "mu:uuid"
    -> "schema:hasAddress"
    -> "foaf:firstName"
    -> "foaf:lastName"
    -> "ext:companyInfo"
    -> "foaf:phone"
    -> "schema:email")
   ("schema:PostalAddress."
    -> "rdf:type"
    -> "mu:uuid"
    -> "schema:addressLocality"
    -> "schema:postalCode"
    -> "schema:streetAddress"))

(supply-allowed-group "logged-in"
  :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
          PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          PREFIX veeakker: <http://veeakker.be/vocabularies/shop/>
          SELECT ?id
          WHERE {
            <SESSION_ID> session:account/^foaf:account ?person.
            ?person mu:uuid ?id.
            FILTER NOT EXISTS {
              ?person veeakker:role veeakker:Administrator.
            }
          } ORDER BY ?id LIMIT 1"
  :parameters (list "id"))

(supply-allowed-group "admin"
  :query "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
          PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          PREFIX veeakker: <http://veeakker.be/vocabularies/shop/>
          SELECT ?id
          WHERE {
            <SESSION_ID> session:account/^foaf:account ?person.
            ?person mu:uuid ?id.
            ?person veeakker:role veeakker:Administrator.
          } ORDER BY ?id LIMIT 1")

(grant (read write)
       :to logged-in-graph
       :for "logged-in")

(grant (read write)
       :to anonymous-session-graph
       :for "anonymous-session")

(grant (read)
       :to-graph (public lfw-extra-info) ;; TODO: rename this graph if this is to persist
       :for-allowed-group "public")
(grant (write)
       :to (public lfw-extra-info) ;; TODO: splitting this up into what is under admin and what is under LFW would clarify
       :for "admin")

(with-scope "service:image-service"
  (grant (read write)
         :to files
         :for "public"))

(with-scope "veeakkerservice:lfw-import"
  (grant (read write)
         :to (public external-identifiers lfw-import-jobs files lfw-extra-info)
         :for "admin"))
