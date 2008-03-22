THIS IS AN UPDATE TO STGSTORE.ZIP, FIRST POSTED TO THE WORLD IN APRIL, 97. IT 
HAS BEEN UPDATED TO USE THE ACTIVEX UNIT IN DELPHI 3 RATHER THAN OLE2 IN DELPHI 2. 
THERE ARE MAJOR DIFFERENCES IN HOW INTERFACES SUCH AS ISTORAGE, ISTREAM, IUNKNOWN 
ARE IMPLEMENTED IN DELPHI 3. IN PARTICULAR, MANY INTERFACES WERE IMPLEMENTED IN 
D2 TO USE VTABLE ADDRESSING. IN D3 THEY ARE USING STDCALL (I SUPPOSE TO SUPPORT
DISPATCHING, MARSHALING, ETC.). SIMPLY USEING THE ACTIVEX UNIT IN D3, THEN COMPILING
WILL NOT WORK. FIRST OFF, IUNKNOWN'S RELEASE METHOD HAS BEEN RENAMED TO _RELEASE.
OTHER METHODS HAVE BEEN RENAMED AS WELL. ALSO, IUNKNOWN HAS NOW BECOME A DELPHI 
SYSTEM RESOURCE. THIS ISN'T SO BAD IN AND OF ITSELF. HOWEVER, CALLING _RELEASE ON
THE ISTORAGE OR ISTREAM INTERFACES WITHIN THE APPLICATION CAUSES ALL KINDS
OF MEMORY ACCESS EXCEPTIONS. IN DELPHI 2 VARIANT STUFF WAS AUTOMATICALLY RELEASED 
WHEN IT WENT OUT OF CONTEXT. I EXTRAPOLATED THIS AND HAVE ASSUMED IN D3, THINGS
LIKE ISTORAGE, ISTREAM ARE RELEASED BY THE SYSTEM WHEN THEY GO OUT OF CONTEXT.
IF ANYONE KNOWS TO THE CONTRARY, PLEASE INFORM ME.


Stgstore.zip is an example of how to use structured storage to store a
combination Treeview/listview structure.


This little application was written as a prototype for a work flow modeling
module within an application I am writing for work. I work for a company that
provides applications to the mortgage and consumer lending industries. One of
our objectives in this application was to provide a form of workflow that would
track the progress of a loan through the lending process. After much analysis,
I and others in my programming team determined that the workflow tools on the
market are fine for tracking and moving documents within an organization, but
not for the kinds of tracking, moving and imposing of business rules we fealt
we needed to provide to our customers. Having made this determination, a
treeview, listview type structure seemed most natural. The next problem was
how best to store the work flow entities, as the number of different
permutations a particular loan can go through are quite large. Each loan will
have it's work flow built as different stages of the lending process is completed.
A database file didn't seem to meet the needs. Thus, the use of structured
storage.

The unit E_StgStr encapsulates the calls to Ole and provides a pascal like
untyped file interface to writing data to a file within the storage file.
Eventually, there will be much more written to the file, streamed objects,
etc.

Any comments or suggestions as to how I might better implement this or other
comments will be greatly appreciated.

David Spies

WORK : davidsp@eastsoft.com
HOME : DSPIES@onecom.com
CS   : 70401,643

