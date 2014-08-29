"""

Automate loading of F-Script Anywhere into any app.
By Daniel Jalkut - @danielpunkass - http://indiestack.com/

Thanks to Daniel Jalkut; I haven't adapted his work at all.  I did
change the comment and whitespace.

See:
- https://gist.github.com/danielpunkass/b868c1488e34210e4d05
- http://indiestack.com/2014/05/f-script-anywhere-with-lldb/

----------------------------------------------------------------

* Make sure you have FScript.framework installed in
    /Library/Frameworks (http://www.fscript.org).

To use:

1. Attach to a process e.g. by "lldb -n TextEdit"
2. Type "fsa" to load an initialize F-Script Anywhere in the context of the app.
3. Type "c" to continue execution of the target app.

"""

def loadFSA(debugger, args, result, internal_dict):
    debugger.HandleCommand('expr (void) [[NSBundle bundleWithPath:@"/Library/Frameworks/FScript.framework"] load]');
    debugger.HandleCommand('expr (void)[FScriptMenuItem insertInMainMenu]');

def __lldb_init_module(debugger, dict):
    debugger.HandleCommand('command script add -f fsa.loadFSA fsa');
