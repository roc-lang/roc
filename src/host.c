#include <stdio.h>
#include <stddef.h>

// External symbol that will be provided by the Roc object file
extern char* _roc_entrypoint(void);

int main(void) {
    // Call the Roc entrypoint to get our string
    char* message = _roc_entrypoint();
    
    if (message != NULL) {
        printf("%s\n", message);
    } else {
        printf("No message received from Roc entrypoint\n");
    }
    
    return 0;
}