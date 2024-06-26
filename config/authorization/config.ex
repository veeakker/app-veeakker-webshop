alias Acl.Accessibility.Always, as: AlwaysAccessible
alias Acl.Accessibility.ByQuery, as: AccessByQuery
alias Acl.GraphSpec.Constraint.Resource.AllPredicates, as: AllPredicates
alias Acl.GraphSpec.Constraint.Resource.NoPredicates, as: NoPredicates
alias Acl.GraphSpec.Constraint.ResourceFormat, as: ResourceFormatConstraint
alias Acl.GraphSpec.Constraint.Resource, as: ResourceConstraint
alias Acl.GraphSpec, as: GraphSpec
alias Acl.GroupSpec, as: GroupSpec
alias Acl.GroupSpec.GraphCleanup, as: GraphCleanup

defmodule Acl.UserGroups.Config do
  @public_resource_types [
    "http://schema.org/Organization",
    "http://veeakker.be/vocabularies/shop/DeliveryPlace",
    "http://veeakker.be/vocabularies/shop/DeliveryKind",
    "http://schema.org/GeoCoordinate",
    "http://schema.org/PostalAddress",
    "http://veeakker.be/vocabularies/shop/ProductGroup",
    "http://schema.org/Product",
    "http://purl.org/goodrelations/v1#Offering",
    "http://purl.org/goodrelations/v1#UnitPriceSpecification",
    "http://purl.org/goodrelations/v1#QuantitativeValue",
    "http://purl.org/goodrelations/v1#TypeAndQuantityNode",
    "http://veeakker.be/vocabularies/shop/SpotlightProduct",
    "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
    "http://mu.semte.ch/vocabularies/ext/Banner",
  ]

  def user_groups do
    [
      # PUBLIC
      %GroupSpec{
        name: "public",
        useage: [:read],
        access: %AlwaysAccessible{},
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/public",
                    constraint: %ResourceConstraint{
                      resource_types: @public_resource_types ++ [
                        # # These can only be set through specific sudo services
                        # "http://veeakker.be/vocabularies/shop/Basket",
                        # "http://veeakker.be/vocabularies/shop/OrderLine",
                        # "http://mu.semte.ch/vocabularies/session/Session",
                        # "http://mu.semte.ch/vocabularies/ext/Favourite",
                        # "http://xmlns.com/foaf/0.1/Person",
                        # "http://xmlns.com/foaf/0.1/OnlineAccount",
                        # "http://veeakker.be/vocabularies/shop/Address"
                    ],
                      inverse_predicates: %AllPredicates{}
                    } } ] },
      # ADMIN
      %GroupSpec{
        name: "admin",
        useage: [:write, :read_for_write],
        access: %AccessByQuery{
                  vars: [],
                  query: "PREFIX session: <http://mu.semte.ch/vocabularies/session/>
                    PREFIX veeakker: <http://veeakker.be/vocabularies/shop/>
                    PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                    SELECT ?id WHERE {
                    <SESSION_ID> session:account/^foaf:account ?person.
                    GRAPH <http://veeakker.be/graphs/administrators> {
                      ?person veeakker:role veeakker:Administrator.
                    }
                  }
                " },
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/public",
                    constraint: %ResourceConstraint{
                      resource_types: @public_resource_types,
                      inverse_predicates: %AllPredicates{}
                    } } ]
      },
      
      # CLEANUP
      #
      %GraphCleanup{
        originating_graph: "http://mu.semte.ch/application",
        useage: [:write],
        name: "clean"
      }
    ]
  end
end
