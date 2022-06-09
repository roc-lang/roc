extern int js_called_directly_from_roc(int);
extern int js_called_indirectly_from_roc(int);
extern int js_called_directly_from_main(int);
extern int js_called_indirectly_from_main(int);
extern int js_unused(int);

int host_internal_state = 0;

int host_called_indirectly_from_roc(int x)
{
    return host_internal_state++;
}

int host_called_directly_from_roc(int x)
{
    return host_called_indirectly_from_roc(x) + js_called_indirectly_from_roc(2);
}

int host_called_indirectly_from_main(int x)
{
    return (host_internal_state++) * 4;
}

int host_called_directly_from_main(int x)
{
    return host_called_indirectly_from_main(x) + js_called_indirectly_from_main(16);
}

int host_unused(int x)
{
    // Call some functions from here to get them included in the output file,
    // without having to do a more complicated build
    return (host_internal_state++) + js_unused(123) + js_called_directly_from_roc(456);
}

int main(int argc, char **argv)
{
    return host_called_directly_from_main(argc) + js_called_directly_from_main(argc);
}
