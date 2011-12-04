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
        :plists, you probably want to also set
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

For example

    CL-USER> (defvar *json-string* "[{\"foo\":1,\"bar\":[7,8,9]},2,3,4,[5,6,7],true,null]")
    *JSON-STRING*
    CL-USER&gt; (let* ((result (json:parse *json-string*)))
               (print result)
               (alexandria:hash-table-plist (first result)))
    
    (#&lt;HASH-TABLE :TEST EQUAL :COUNT 2 {5A4420F1}&gt; 2 3 4 (5 6 7) T NIL) 
    ("bar" (7 8 9) "foo" 1)
    CL-USER&gt; (defun maybe-convert-to-keyword (js-name)
               (or (find-symbol (string-upcase js-name) :keyword)
                   js-name))
    MAYBE-CONVERT-TO-KEYWORD
    CL-USER&gt; :FOO ; intern the :FOO keyword
    :FOO
    CL-USER&gt; (let* ((json:*parse-json-arrays-as-vectors* t)
                    (json:*parse-json-booleans-as-symbols* t)
                    (json:*parse-object-key-fn* #'maybe-convert-to-string)
                    (result (json:parse *json-string*)))
               (print result)
               (alexandria:hash-table-plist (aref result 0)))
    
    #(#&lt;HASH-TABLE :TEST EQUAL :COUNT 2 {59B4EAD1}&gt; 2 3 4 #(5 6 7) YASON:TRUE NIL) 
    ("bar" #(7 8 9) :FOO 1)