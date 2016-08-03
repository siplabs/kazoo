/*
Section: Crossbar
Title: Units
Language: en-US
*/

Units allows to link physical phones with Kazoo devices and also contain other settings for the phones.
The API is used to get information about the units, and also create, modify and delete them.

# Units

## cURL examples

### Summary of available units

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/units

### Add a unit

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/units -d '{"name": "unit name", "mac_address": "0123456789ab"}'

### Get a specific unit's information

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/units/{UNIT_ID}

### Update a unit's definition

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/units/{UNIT_ID} -d '{"name": "new unit name", "mac_address": "0123456789ab"}'

### Delete a unit

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/units/{UNIT_ID}

The simplest unit possible contains only name, mac address, and a single line, that connects the phone line and a kazoo device. Here is given the example of such a unit.

    {
        "mac_address":"1122334455aa",
        "name":"Bills Phone",
        "lines":[
            {
                "line_number": "1",
                "device": "9b759347e160679e6b215449a131adbd"
            }
        ],
    }

For the more complex cases unit could contain some configuration parameters for example the codecs settings and an address book as shown here:

    {
        "mac_address":"1122334455aa",
        "name":"Bills Phone",
        "lines":[
            {
                "line_number": "1",
                "device": "9b759347e160679e6b215449a131adbd"
            }
        ],
        config:{
           "voice": {
               "audioProfile": {
                   "G711Mu": {
                       "jitterBufferMax": "170",
                       "jitterBufferMin": "60",
                       "jitterBufferShrink": "520",
                       "payloadSize": "40"
                   }
               }
           },
           "phone_book": [
               {
                   "first_name": "Mike",
                   "second_name": "Johnson",
                   "phone_number": "2001"
               },
               {
                   "first_name": "Jack",
                   "second_name": "Jones",
                   "phone_number": "2002"
               }
           ]
        }
    }

