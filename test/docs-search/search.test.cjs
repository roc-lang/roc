const test = require('node:test');
const assert = require('node:assert/strict');
const {rank, snippet} = require('../../src/docs/static/search-core.js');
const entries = [
 {title:'Some example', text:'A := B and Token :: Str', href:'example/', category:'guides'},
 {title:'Types › Nominal Types', text:'A nominal type uses :=', terms:[':='], href:'types/#nominal-types',category:'guides'},
 {title:'Types › Opaque Nominal Types', text:'An opaque type uses ::',terms:['::'],href:'types/#opaque-nominal-types',category:'guides'},
 {title:'List.fold',text:'Combine elements into an accumulator',terms:['lreduce'],href:'List/#fold',category:'api'}
];
test('curated punctuation outranks incidental code mentions',()=>{
 assert.equal(rank(entries,':=')[0].href,'types/#nominal-types');
 assert.equal(rank(entries,'::')[0].href,'types/#opaque-nominal-types');
 assert.equal(rank(entries,'::').length,1);
 assert.equal(rank(entries,':=').length,1);
});
test('heading and prose searches are case-insensitive',()=>{
 assert.equal(rank(entries,'Nominal')[0].href,'types/#nominal-types');
 assert.equal(rank(entries,'OPAQUE')[0].href,'types/#opaque-nominal-types');
 assert.equal(rank(entries,'elements accumulator')[0].href,'List/#fold');
});
test('synonyms select explicit API destinations',()=>{
 assert.equal(rank(entries,'lreduce')[0].href,'List/#fold');
 assert.equal(rank(entries,'lreduce')[0].exactTerm,true);
});
test('empty and absent queries have no results',()=>{
 assert.deepEqual(rank(entries,' '),[]);
 assert.deepEqual(rank(entries,'missing'),[]);
});
test('snippets contain the match and remain bounded',()=>{
 const text='prefix '.repeat(50)+'needle '+'suffix '.repeat(50);
 assert.ok(snippet(text,'needle').includes('needle'));
 assert.ok(snippet(text,'needle').length<=172);
});

test('multiple sources for one destination yield one result',()=>{
 const duplicate = {...entries[1],text:'Different source containing nominal',terms:[]};
 const results = rank([...entries,duplicate],'nominal');
 assert.equal(results.filter(item=>item.href===entries[1].href).length,1);
});
