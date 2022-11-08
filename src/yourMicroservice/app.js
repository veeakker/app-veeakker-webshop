import { app, query } from 'mu';

app.get('/', (_req, res) =>
  res.send(JSON.stringify({ message: `This is template speaking` }))
);

app.get('/hello', async function( req, res ) {
  debugger;
  res.send(JSON.stringify({ message: `Hello world!`}));
});

app.get('/query', async function( req, res ) {
  debugger;
  const queryResponse = await query(`SELECT (COUNT (*) AS ?count) WHERE { ?s ?p ?o }`);
  debugger;
  res.send(JSON.stringify({ message: `Amount of triples found: ${queryResponse.results.bindings[0].count.value}`}));
});
