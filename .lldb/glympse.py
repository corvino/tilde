gMetadata = lldb.frame.FindVariable ("dictionary")
object = gMetadata.GetChildMemberWithName('_object')
type = object.GetChildMemberWithName('_type').GetValueAsSigned(0)

gHashContainer = object.GetChildMemberWithName('_hashtable').GetChildMemberWithName('_object').GetChildMemberWithName('_map')
gHash = gHashContainer.GetChildAtIndex(0)

# command script import ~/.lldb/glympse.py
# p gMetadata

import json
from StringIO import StringIO

def ShowPrimitive(debugger,user_input,result,unused):
    thread = debugger.GetSelectedTarget().GetProcess().GetSelectedThread()
    frame = thread.GetFrameAtIndex(0)
    primitive = frame.FindVariable(user_input)


def GlyCard_Metadata(debugger,user_input,result,unused):
    thread = debugger.GetSelectedTarget().GetProcess().GetSelectedThread()
    frame = thread.GetFrameAtIndex(0)
    for arg in frame.args:
        print >>result,"arg: " + str(arg.name)

def debugdict(pDict):
    print pDict.GetNumChildren()
    for i in range(0,pDict.GetNumChildren()):
        print pDict.GetChildAtIndex(i).GetName()
    stdhash = pDict.GetChildAtIndex(0)
    print stdhash.GetNumChildren()


def GPrimitiveToPython(gprim):
    object = gprim.GetChildMemberWithName('_object')
    type = object.GetChildMemberWithName('_type').GetValueAsSigned(0)

    if 2 == type:
        pDict = {}
        gHashContainer = object.GetChildMemberWithName('_hashtable').GetChildMemberWithName('_object').GetChildMemberWithName('_map')
        gHash = gHashContainer.GetChildAtIndex(0)

        print ("ghashContainer # = " + str(gHashContainer.GetNumChildren()))
        print ("gHash # = " + str(gHashContainer.GetNumChildren()))

        print "gHashContainer pointer: " + str(gHashContainer.TypeIsPointerType())
        print "gHash pointer: " + str(gHash.TypeIsPointerType())

        #print gHashContainer
        #print gHash

        # This crashes!!!
        # value1 = gHash.GetChildAtIndex(0)

        #debugdict(gHashContainer)
        #print(gHash)

        #gtable = GHashTableToPython(object.GetChildMemberWithName('_hashtable'))
        return pDict
    else:
        return "NONE!"

def GPrimitive(valobj,internal_dict):
    # type = valobj.GetChildMemberWithName('_object').GetChildMemberWithName('_type')

    # return "type: " + str(type)
    # #return str(valobj.GetNumChildren())
    primitive = GPrimitiveToPython(valobj)
    return str(primitive)

def __lldb_init_module(debugger, dict):
    debugger.HandleCommand('command script add -f glympse.GlyCard_Metadata gcj');
    debugger.HandleCommand('type summary add -F glympse.GPrimitive Glympse::GPrimitive')
