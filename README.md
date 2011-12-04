YASON
=====

YASON is a Common Lisp library for encoding and decoding data in
the [JSON](http://json.org/) interchange format.
JSON is used in AJAX applications as a lightweight alternative
to XML.  YASON has the sole purpose of encoding and decoding
data and does not impose any object model on the Common Lisp
application that uses it.

Introduction
------------

[JSON](http://json.org/) is an established alternative
to XML as a data interchange format for web applications.  YASON
implements reading and writing of JSON formatted data in Common
Lisp.  It does not attempt to provide a mapping between CLOS
objects and YASON, but can be used to implement such mappings.

[CL-JSON](http://common-lisp.net/project/cl-json/) is
another Common Lisp package that can be used to work with JSON
encoded data.  It takes a more integrated approach, providing
for library internal mappings between JSON objects and CLOS
objects.  YASON was created as a lightweight, documented
alternative with a minimalistic approach and extensibilty.

Download and Installation
-------------------------

YASON has its permanent home at
[common-lisp.net](http://common-lisp.net/project/yason/).
It can be obtained by downloading the
[release tarball](http://common-lisp.net/project/yason/files/yason.tar.gz).
The current release is 0.2.

You may also check out the current development version from its
[git repository](http://github.com/hanshuebner/yason/). If you
have suggestions regarding YASON, please email me at
*hans.huebner@gmail.com*.

YASON is written in ANSI Common Lisp and does not depend on
other libraries.

YASON lives in the **:yason** package and creates a package nickname
**:json**.  Applications will not normally **:use** this
package, but rather use qualified names to access YASON's
symbols.  For that reason, YASON's symbols do not contain the
string "JSON" themselves.  See below for usage samples.

Mapping between JSON and CL datatypes
-------------------------------------

By default, YASON performs the following mappings between JSON and
CL datatypes:
<table border="1">
  <thead>
    <tr>
      <th>JSON<br/>datatype</th>
      <th>CL<br/>datatype</th>
      <th>Notes</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>object</td>
      <td>hash-table<br/>:test&#x00A0;#'equal</td>
      <td>
        Keys are strings by default, see
        <b>*parse-object-key-fn*</b>.  Set
        <b>*parse-object-as*</b> to <b>:alist</b>
        value in order to have YASON parse objects as alists or to
        <b>:plist</b> to parse them as plists.  When using
        plists, you probably want to also set
        <b>*parse-object-key-fn*</b> to a function
        that interns the object's keys to symbols.
      </td>
    </tr>
   <tr>
      <td>array</td>
      <td>list</td>
      <td>
        Can be changed to read to vectors,
        see <b>*parse-json-arrays-as-vectors*</b>
      </td>
    </tr>
    <tr>
      <td>string</td>
      <td>string</td>
      <td>
        JSON escape characters are recognized upon reading.
        Upon writing, known escape characters are used, but
        non-ASCII Unicode characters are written as is.
      </td>
    </tr>
    <tr>
      <td>number</td>
      <td>number</td>
      <td>
        Parsed with READ, printed with PRINC.  This is not a
        faithful implementation of the specification.
      </td>
    </tr>
    <tr>
      <td>true</td>
      <td>t</td>
      <td>Can be changed to read as TRUE, see <b>*parse-json-booleans-as-symbols*</b></td>
    </tr>
     <tr>
      <td>false</td>
      <td>nil</td>
      <td>Can be changed to read as FALSE, see <b>*parse-json-booleans-as-symbols*</b></td>
    </tr>
    <tr>
      <td>null</td>
      <td>nil</td>
      <td></td>
    </tr>
  </tbody>
</table>

Parsing JSON data
-----------------

JSON data is always completely parsed into an equivalent
in-memory representation.  Upon reading, some translations are
performed by default to make it easier for the Common Lisp
program to work with the data; see <b>mapping</b>
for details.  If desired, the parser can be configured to
preserve the full semantics of the JSON data read.

For exampler

    CL-USER> (defvar *json-string* "[{\"foo\":1,\"bar\":[7,8,9]},2,3,4,[5,6,7],true,null]")
    *JSON-STRING*
    CL-USER> (let* ((result (json:parse *json-string*)))
               (print result)
               (alexandria:hash-table-plist (first result)))
    
    (#<HASH-TABLE :TEST EQUAL :COUNT 2 {5A4420F1}> 2 3 4 (5 6 7) T NIL) 
    ("bar" (7 8 9) "foo" 1)
    CL-USER> (defun maybe-convert-to-keyword (js-name)
               (or (find-symbol (string-upcase js-name) :keyword)
                   js-name))
    MAYBE-CONVERT-TO-KEYWORD
    CL-USER> :FOO ; intern the :FOO keyword
    :FOO
    CL-USER> (let* ((json:*parse-json-arrays-as-vectors* t)
                    (json:*parse-json-booleans-as-symbols* t)
                    (json:*parse-object-key-fn* #'maybe-convert-to-string)
                    (result (json:parse *json-string*)))
               (print result)
               (alexandria:hash-table-plist (aref result 0)))
    
    #(#<HASH-TABLE :TEST EQUAL :COUNT 2 {59B4EAD1}> 2 3 4 #(5 6 7) YASON:TRUE NIL) 
    ("bar" #(7 8 9) :FOO 1)

The second example modifies the parser's behaviour so that JSON
arrays are read as CL vectors, JSON booleans will be read as the
symbols TRUE and FALSE and JSON object keys will be looked up in
the **:keyword** package.  Interning strings coming from an
external source is not recommended practice.

### Parser dictionary ####

**parse** *input* => *object*

Parse *input*, which needs to be a string
or a stream, as JSON.  Returns the Lisp representation of the
JSON structure parsed.

**\*parse-jason-arrays-as-vectors\***

If set to a true value, JSON arrays will be parsed as vectors,
not as lists.

**\*parse-object-as\***

Can be be set to **:hash-table** to parse objects as hash
tables, **:alist** to parse them as alists or
**:plist** to parse them as plists.  **:hash-table**
is the default.

**\*parse-json-booleans-as-symbols\***

If set to a true value, JSON booleans will be read as the
symbols TRUE and FALSE, not as T and NIL, respectively.

**\*parse-object-key-fn\***

Function to call to convert a key string in a JSON array to a
key in the CL hash produced.

Encoding JSON data
------------------

YASON provides for two distinct modes to encode JSON data:
Applications can either create an in memory representation of the
data to be serialized, then have YASON convert it to JSON in one
go, or they can use a set of macros to serialze the JSON data
element-by-element, thereby having fine grained control over the
layout of the generated data.

### Encoding a JSON DOM ###

In this mode, an in-memory structure is encoded in JSON format.
The structure must consist of objects that are serializable
using the **ENCODE** function.  YASON defines a
number of encoders for standard data types
(see **MAPPING**), but the application can
define additional methods, e.g. for encoding CLOS objects.

For example:

    CL-USER> (json:encode 
              (list (alexandria:plist-hash-tablenn
                     '("foo" 1 "bar" (7 8 9))
                     :test #'equal)
                    2 3 4
                    '(5 6 7)
                    t nil)
              *standard-output*)
    [{"foo":1,"bar":[7,8,9]},2,3,4,[5,6,7],true,nunll]
    (#<HASH-TABLE :TEST EQUAL :COUNT 2 {59942D21}> 2 3 4 (5 6 7) T NIL)

### DOM encoder dictionary ###

**encode** *object &optional stream* => *object*

Encode *object* to *stream* in JSON format.  May be
specialized by applications to perform specific
rendering.  *stream* defaults to *STANDARD-OUTPUT*.

Encoding JSON in streaming mode
-------------------------------

In this mode, the JSON structure is generated in a stream.
The application makes explicit calls to the encoding library
in order to generate the JSON structure.  It provides for more
control over the generated output, and can be used to generate
arbitary JSON without requiring that there exists a directly
matching Lisp datastructure.  The streaming API uses
the **encode** function, so it is possible to
intermix the two.  See **app-encoders** for
an example.

For example:

    CL-USER> (json:with-output (*standard-output*)
               (json:with-array ()
                 (dotimes (i 3)
                   (json:encode-array-element i))))
    [0,1,2]
    NIL
    CL-USER> (json:with-output (*standard-output*)
               (json:with-object ()
                 (json:encode-object-element "hello" "hu hu")
                 (json:with-object-element ("harr")
                   (json:with-array ()
                     (dotimes (i 3)
                       (json:encode-array-element i))))))
    {"hello":"hu hu","harr":[0,1,2]}
    NIL

### Streaming encoder dictionary ###

**with-output\** *(stream) &body body* => *result\**

Set up a JSON streaming encoder context on *stream*,
then evaluate *body*.

**with-output-to-string\*** *&body body* => *result\**

Set up a JSON streaming encoder context, then
evaluate *body*.  Return a string with the
generated JSON output.

**no-json-output-context**

This condition is signalled when one of the stream encoding
functions is used outside the dynamic context of a
**WITH-OUTPUT** or **WITH-OUTPUT-TO-STRING\*** body.

**with-array** *&body body* => *result\**

Open a JSON array, then run *body*.  Inside
the body, **ENCODE-ARRAY-ELEMENT** must be
called to encode elements to the opened array.  Must be called
within an existing JSON encoder context, see
**WITH-OUTPUT** and **WITH-OUTPUT-TO-STRING\***.

**encode-array-element** *object* => *object*

Encode *object* as next array element to
the last JSON array opened
with **WITH-ARRAY** in the dynamic
context.  *object* is encoded using the
**ENCODE** generic function, so it must be of
a type for which an **ENCODE** method is
defined.

**with-object** *&body body* => *result\**

Open a JSON object, then run *body*.  Inside the body,
**ENCODE-OBJECT-ELEMENT**
or **WITH-OBJECT-ELEMENT** must be called to
encode elements to the object.  Must be called within an
existing JSON encoder context,
see **WITH-OUTPUT**
and **WITH-OUTPUT-TO-STRING***.

**with-object-element** *&body body* => *result\**

Open a new encoding context to encode a JSON object
element.  *key* is the key of the element.
The value will be whatever *body*
serializes to the current JSON output context using one of the
stream encoding functions.  This can be used to stream out
nested object structures.

**encode-object-element** *key value* => *value*

Encode *key* and *value*
as object element to the last JSON object opened
with **WITH-OBJECT** in the dynamic
context.  *key*
and *value* are encoded using
the **ENCODE** generic function, so they both
must be of a type for which an **ENCODE**
method is defined.

### Application specific encoders ###

Suppose your application uses structs to represent its data, and
you want to encode such structs using JSON in order to send it
to a client application.  Suppose further that your structs also
include internal information that you do not want to send.  Here
is some code that illustrates how one could implement a
serialization function:

    CL-USER> (defstruct user name age password)
    USER
    CL-USER> (defmethod json:encode ((user user) &optional (stream *standard-output*))
               (json:with-output (stream)
                 (json:with-object ()
                   (json:encode-object-element "name" (user-name user))
                   (json:encode-object-element "age" (user-age user)))))
    #<STANDARD-METHOD YASON:ENCODE (USER) {5B40A591}>
    CL-USER> (json:encode (list (make-user :name "horst" :age 27 :password "puppy")
                                (make-user :name "uschi" :age 28 :password "kitten")))
    [{"name":"horst","age":27},{"name":"uschi","age":28}]
    (#S(USER :NAME "horst" :AGE 27 :PASSWORD "puppy")
     #S(USER :NAME "uschi" :AGE 28 :PASSWORD "kitten"))

As you can see, the streaming API and the DOM encoder can be
used together.  **ENCODE** invokes itself
recursively, so any application defined method will be called
while encoding in-memory objects as appropriate.

License
-------
    Copyright (c) 2008 Hans Huebner
    All rights reserved.
    
    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:
    
      - Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
    
      - Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in
        the documentation and/or other materials provided with the
        distribution.
    
      - Neither the name BKNR nor the names of its contributors may be
        used to endorse or promote products derived from this software
        without specific prior written permission.
    
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Acknowledgements
----------------
Thanks go to Edi Weitz for being a great inspiration.
Thanks to David Lichteblau for coining YASON's name. 
