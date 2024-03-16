# Improvements to the language server.

## Performance
- [ ] Implement some performance logging for actions like completion goto def hover etc

### Completion  
Currently the way we handle documentation and type info for completion requires us to prform all the computation up front and has no caching. Documentation is also quite inneficient and likely requires a lot of repeated computation which could be slow in files with lots of doc comments.  The language server allows us to defer getting the info for a completion until the item is actually selected in the editor, this could speed up completion requests. 
We would need to profile this to see how performant it really is.

## Features
- [ ] Rename refactoring #HighPriority
- [ ] Show references #HighPriority
	Initially this could just be within the current file and it could be expanded to multi file
	Should have a lot in commmon with rename refactoring
- [ ] Completion within the import section 

### Code Actions
- [ ] Create cases of when is block  
- [ ] Destructure record
- [ ] Extract selection into it's own function (This one seems hard)
- [ ] Add function to exposed list 

### Completion
- [ ] Completion of Tags #HighPriority
- [ ] Completion of Types inside signatures

- [ ] Completion of when is cases
- [ ] Completion of record fields 
	- [ ] During destructuring
	- [ ] When creating records
	- [ ] When describing records inside function params

- [ ] Completion of unimported vars that are exposed by modules within the project (will need to have appropriate indicator and ranking so as not to be annoying)
 
 



	
