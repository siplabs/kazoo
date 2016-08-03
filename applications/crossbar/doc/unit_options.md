/*
Section: Crossbar
Title: Unit options
Language: en-US
*/

Opts is used to get a description of all the parameters possible to configure for a some particular phone model.
Also you can create such descriptions using this API.


# Opts

## General URL syntax rules
First of all the unit options API is not connected to the accounts in other worlds the phone descriptions it allows to manage are common for all the accounts.
That means all the URLs that used to make the API requests does not contain '/accounts/{ACCOUNT_ID}/' part which is typical for most other crossbar APIs.
An URL consists of three parts: first is path to API itself 'http://server.com:8000/v1/account_provision'. Second is path to the resource you want to manage.
That could be just root '/' or any of '/vendor', '/vendor/family', 'vendor/family/model'. For example '/polycom' or 'yealink/t2x/t22p'.
First two parts are compulsory, third one is optional. Third part is keyword that points on operation you want to do, that could be '_config' or '_hiconfig'.
The example complete URL is:

    http://server.com:8000/v1/unit_options/polycom/sond_point/ip670/_hiconfig

The meaning of this URL will be explained later.

## cURL examples

There are three types of operations. First is navigation on hierarchy which is looks like a navigation on directory tree with file manager.

### List all the vendors

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/unit_options/

### List all the phone models for Polycom SoundPoint family

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/unit_options/polycom/sound_point

Also you can add elements in hierarchy

### Add the t2x family for Yealink vendor

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}"  http://server.com:8000/v1/unit_options/yalink/t2x -d '{"!_config":{...}}'

The "!_config" key contains description of parameters which is common for all the t2x family.
This key is mandatory but could be empty. It's impossible to set this key for the root of the hierarchy.
The example of the "!_config" object with explanations will be given at the end of the document.
Then you want to add some level of hierarchy all the previous levels must exist in other words to add a phone model you need to have a vendor and a family created.

### Modify a vendor description

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}"  http://server.com:8000/v1/unit_options/vendor -d '{"!_config":{...}}'

### Delete a phone's description

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/unit_options/vendor/family/phone

### Delete a vendor with all the families and phones

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/unit_options/vendor/

### Get description of all the parameters for a family

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/unit_options/vendor/family/_config

You can get "!_config" for any level of hierarchy except a root.

### Get summarized description of all the parameters for some phone model

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/unit_options/vendor/family/model/_hiconfig

Summarized means that the parameters of the vendor family and model would be taken into account to build final description.
The rules of summarizing are: if there is same key in parent and child accounts the value of the key would be taken from child one.
If a parent's account key doesn't exist in a child account the key value pair would be added to child account.
In other words family can redefine any parameters defined for vendor and so on.
That rules applied for all the tree consequently starting from the vendor, the complex keys are processing recursively.

You can also you "_hiconfig" for a family, for a vendor the result would be same as for "_config" keyword.


## The "!_config" object example
The "!_config" contains the description of the parameters in JSON format.
It uses http://json-schema.org/draft-03/schema# as specification.
The example for some network parameters is given here:

    "network": {
                "access_level": {
                    "description": "Access level",
                    "enum": [
                        "admin",
                        "user"
                    ],
                    "name": "Access level",
                    "required": true,
                    "type": "enum"
                },
                "advanced": {
                    "802.1x": {
                        "description": "802.1x Authentication Configuration",
                        "identity": {
                            "description": "Identity",
                            "name": "Identity",
                            "required": true,
                            "type": "string"
                        },
                        "md5_password": {
                            "description": "MD5 Password",
                            "name": "MD5 Password",
                            "required": true,
                            "type": "string"
                        },
                        "mode": {
                            "description": "802.1x Mode",
                            "name": "802.1x Mode",
                            "required": true,
                            "type": {
                                "enum": [
                                    "Disabled",
                                    "EAP-MD5",
                                    "EAP-TLS",
                                    "EAP-PEAP/MSCHAPv2",
                                    "EAP-TTLS/EAP-MSCHAPv2",
                                    "EAP-PEAP/GTC",
                                    "EAP-TTLS/EAP-GTC"
                                ]
                            }
                        },
                        "name": "802.1x",
                        "required": true,
                        "type": "object"
                    },
                    "description": "Advanced network configuration parametrs",
                    "local_rtp_port": {
                        "description": "Define voice transmission port",
                        "max_rtp_port": {
                            "description": "Max RTP Port (1~65535)",
                            "max_value": "65535",
                            "min_value": "1",
                            "name": "Max RTP Port",
                            "required": true,
                            "type": "integer"
                        },
                        "min_rtp_port": {
                            "description": "Min RTP Port (1~65535)",
                            "max_value": "65535",
                            "min_value": "1",
                            "name": "Min RTP Port",
                            "required": true,
                            "type": "integer"
                        },
                        "name": "Local RTP Port",
                        "required": true,
                        "type": "object"
                    }
                }
    }