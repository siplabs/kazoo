/*
Section: CCCP
Title: Calling Card Callback Platform
Language: en-US
*/

CCCP
----

Create cccp application configuration in system_configs

Edit system_config db's cccp doc:
```
    "cccp_cb_number": "7123456789" - callback number
    "cccp_cc_number": "7098765432" - calling card dial-in number
    "tries_count": 3, - number of tries before number is locked
    "ensure_valid_caller_id": true,
    "default_caller_id_number": "7XXXXXXXXXX",
    "allowed_callee_regex": "^\\+?\\d{7,}$",
    "callflow_number": "cccp_handler" - callflow number to lookup within account
    "platform_collect_timeout": {{timeout}}
    "platform_interdigit_timeout": 5000 - timeout between digits
    "platform_long_pin_length": 10 - long PIN length. After collecting the 'platform_long_pin_length' numbers the processing will be started immediately without any timeout. Set it to 0 (zero) if no long PIN should be.
    "platform_short_pin_length": 4 - short PIN length. After collecting the 'platform_short_pin_length' numbers the processing will be started after the 'platform_short_pin_timeout' timeout. Set it to 0 (zero) if no short PIN should be.
    "platform_short_pin_timeout": 2000 - timeout executed after collecting the short PIN
```

Add PIN auth:
```
   PUT: /v1/accounts/{{account_id}}/cccps
    {
        "data":{
            "pin": "0192837465",
            "outbound_cid":"+0987654321",
            "active": true
        }
    }
```
Add CID auth:

```
PUT: /v1/accounts/{{account_id}}/cccps
    {
        "data":{
            "cid": "1234567890",
            "outbound_cid":"+0987654321",
            "active": true
        }
    }
```
Add CID+PIN auth:

```
PUT: /v1/accounts/{{account_id}}/cccps
    {
        "data":{
            "pin": "0192",
            "cid": "1234567890",
            "outbound_cid":"+0987654321",
            "active": true
        }
    }
```

PIN length should be 4-12 digits
