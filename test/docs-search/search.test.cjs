const test = require('node:test');
const assert = require('node:assert/strict');
const {rankApi, rank, snippet, highlightParts} = require('../../src/docs/static/search-core.js');
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

test('aliases match every prefix and preserve their full display label',()=>{
 for (let length=1;length<='lreduce'.length;length++) {
  const match=rank(entries,'LREDUCE'.slice(0,length))[0];
  assert.equal(match.href,'List/#fold');
  assert.equal(match.matchedTerm,'lreduce');
  assert.equal(match.exactTerm,length==='lreduce'.length);
 }
});
test('partial punctuation does not activate curated symbols',()=>{
 assert.ok(rank(entries,':').every(entry=>!entry.matchedTerm));
});

test('snippets highlight separate words case-insensitively without interpreting markup',()=>{
 const text='<b>JSON</b> and parsing json';
 const parts=highlightParts(text,'json parsing');
 assert.equal(parts.map(p=>p.text).join(''),text);
 assert.deepEqual(parts.filter(p=>p.match).map(p=>p.text),['JSON','parsing','json']);
});
test('highlighting treats operator and regex characters literally',()=>{
 for(const query of ['?', '??', '|>', '[]', '+', '.', '*', '(', '\\']) {
  assert.deepEqual(highlightParts('a '+query+' b',query).filter(p=>p.match).map(p=>p.text),[query]);
 }
 assert.deepEqual(highlightParts('text','  '),[{text:'text',match:false}]);
});
test('multiword snippets center on a matching word even without the exact phrase',()=>{
 const result=snippet('prefix '.repeat(50)+'JSON input needs careful parsing','json parsing');
 assert.ok(result.includes('JSON'));
});


test('API matches suppress partial aliases but retain exact aliases and ordinary matches',()=>{
 const docs = [
  {title:'List.fold_rev',text:'Fold from the right',terms:['foldr'],href:'List/#fold_rev',category:'api'},
  {title:'List.fold',text:'Combine elements',terms:['lreduce'],href:'List/#fold',category:'api'}
 ];
 const options = {allowAliasPrefixes:false};
 assert.ok(rank(docs,'fol',options).every(item=>!item.matchedTerm));
 assert.equal(rank(docs,'fol',options).length,2);
 assert.equal(rank(docs,'foldr',options)[0].matchedTerm,'foldr');
 assert.deepEqual(rank(docs,'lredu',options),[]);
 assert.equal(rank(docs,'lredu',{allowAliasPrefixes:true})[0].matchedTerm,'lreduce');
});


test('API type names precede function names, which precede signature-only matches',()=>{
 const api = [
  {name:'alpha.consume',kind:'value',signature:':list(u8)->u64'},
  {name:'str.to_list',kind:'value',signature:':str->list(u8)'},
  {name:'list.list',kind:'type',signature:':=opaque'},
  {name:'list.len',kind:'value',signature:':list(a)->u64'},
  {name:'other.container',kind:'type',signature:':=list(u8)'},
  {name:'other.list_view',kind:'type',signature:':=opaque'},
  {name:'str.len',kind:'value',signature:':str->u64'}
 ];
 assert.deepEqual(rankApi(api,' LIST ').map(x=>x.name),[
  'list.list','other.list_view','str.to_list','list.len','alpha.consume','other.container'
 ]);
 assert.deepEqual(rankApi(api,'str.to_list'),[api[1]]);
 assert.deepEqual(rankApi(api,'List( U8 )'),[api[0],api[1],api[4]]);
 assert.deepEqual(rankApi(api,'len').map(x=>x.name),['list.len','str.len']);
 assert.deepEqual(rankApi(api,''),[]);
 assert.deepEqual(rankApi(api,'missing'),[]);
});
