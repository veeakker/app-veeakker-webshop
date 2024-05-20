;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

(add-delta-logger)
(add-delta-messenger "http://delta-notifier/")

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* t)
(setf *backend* "http://triplestore:8890/sparql")

(in-package :server)
(setf *log-incoming-requests-p* t)


;;;;;;;;;;;;;;;;;
;;; access rights
(in-package :acl)

(define-prefixes
  :ext "http://mu.semte.ch/vocabularies/ext/"
  :gr "http://purl.org/goodrelations/v1#"
  :nfo "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#"
  :schema "http://schema.org/"
  :service "http://services.semantic.works/"
  :shop "http://veeakker.be/vocabularies/shop/"
  :skos "http://www.w3.org/2004/02/skos/core#")

(define-graph public ("http://mu.semte.ch/graphs/public")
  ("schema:Organization" -> _ <- _)
  ("shop:DeliveryPlace"  -> _ <- _)
  ("shop:DeliveryKind" -> _ <- _)
  ("schema:GeoCoordinate" -> _ <- _)
  ("schema:PostalAddress" -> _ <- _)
  ("shop:ProductGroup" -> _ <- _)
  ("schema:Product" -> _ <- _)
  ("gr:Offering" -> _ <- _)
  ("gr:UnitPriceSpecification" -> _ <- _)
  ("gr:QuantitativeValue" -> _ <- _)
  ("gr:TypeAndQuantityNode" -> _ <- _)
  ("shop:SpotlightProduct" -> _ <- _)
  ("nfo:FileDataObject" -> _ <- _)
  ("ext:Banner" -> _ <- _))

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

(grant (read)
       :to-graph public
       :for-allowed-group "public")
(grant (write)
       :to public
       :for "admin")

(with-scope "http://services.semantic.works/image-service"
  (grant (read write)
         :to files
         :for "public"))
