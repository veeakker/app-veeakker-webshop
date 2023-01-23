import { app, query } from 'mu';

app.get('/', async function( req, res ) {
  let results = await query (
    `
SELECT DISTINCT * WHERE { GRAPH <http://mu.semte.ch/graphs/public> { ?s a <http://veeakker.be/vocabularies/shop/Basket>. ?s <http://veeakker.be/vocabularies/shop/orderLine> ?orderLine. ?orderLine <http://veeakker.be/vocabularies/shop/hasOffering> ?ofering. ?ofering <http://purl.org/goodrelations/v1#hasPriceSpecification> ?priceSpec. ?priceSpec <http://purl.org/goodrelations/v1#hasCurrencyValue> ?price. 

?orderLine <http://veeakker.be/vocabularies/shop/amount> ?pakjes. 

?ofering <http://purl.org/goodrelations/v1#includesObject> ?includedObject. ?includedObject <http://purl.org/goodrelations/v1#amountOfThisGood> ?amount. ?includedObject <http://purl.org/goodrelations/v1#hasUnitOfMeasurement> ?units. 

?product <http://veeakker.be/vocabularies/shop/offerings> ?offering.
?product <http://www.w3.org/ns/adms#identifier> ?plu.
}}
    `
  )
  
  res.send(JSON.stringify(results));} );

