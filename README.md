Swift Generation and Validation
===============================

An Erlang module for testing IBAN numbers.

To start the server use:

    mad com bun iban
    ./iban

The validation example url is:

    http://localhost:7700/iban_server:validate?AT61%201904%203002%203457%203201

or in repl:

    2> iban:is_valid("AT61 1904 3002 3457 3201").
    true

and Result:

    {"AT61 1904 3002 3457 3201",true}

License
-------

Distributed under the terms of ISC License.

Credits
-------

Maxim Sokhatsky
