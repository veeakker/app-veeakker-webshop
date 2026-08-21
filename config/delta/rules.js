export default [
  {
    match: {
      subject: { }
    },
    callback: {
      url: "http://resource/.mu/delta",
      method: "POST"
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 250,
      ignoreFromSelf: true,
      foldEffectiveChanges: true
    }
  },
  {
    match: {
      // listen to all changes
    },
    callback: {
      url: 'http://search/update',
      method: 'POST'
    },
    options: {
      resourceFormat: "v0.0.1",
      gracePeriod: 10000,
      ignoreFromSelf: true,
      foldEffectiveChanges: true
    }
  },
  ...[
    { predicate: { value: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" },
      object: { value: "http://purl.org/goodrelations/v1#Offering" }
    }, {
      predicate: { value: "http://mu.semte.ch/vocabularies/ext/disallowedProductGroup" }
    }, {
      predicate: { value: "http://purl.org/goodrelations/v1#availableAtOrFrom" }
    }, {
      predicate: { value: "http://purl.org/goodrelations/v1#offers" }
    }, {
      predicate: { value: "http://veeakker.be/vocabularies/shop/disallowedProductGroup" }
    }, {
      predicate: { value: "http://veeakker.be/vocabularies/shop/hasSupplier" }
    }, {
      predicate: { value: "http://veeakker.be/vocabularies/shop/hasDeliveryPlace" }
    }, {
      predicate: { value: "http://veeakker.be/vocabularies/shop/offerings" }
    }].map( (match) => ({
    match,
    callback: {
      // the service could become smarter and base itself on actual delta messages
      url: "http://product-availability-distribution/distribute",
      method: "POST"
    },
    options: {
      resoureFormat: "v0.0.1", // not used
      gracePeriod: 1000,
      ignoreFromSelf: true, // the service writes gr:availableAtOrFrom itself, which would re-trigger /distribute
      foldEffectiveChanges: true
    }
  }))
];
