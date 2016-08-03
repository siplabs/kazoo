/*
Section: Crossbar
Title: Profiles
Language: en-US
*/

# Profiles
A profile is sets of policies what could be applied to any unit of the account.
The profiles are used in case when you have some common configuration settings for more than one unit (policies)
and you need to group the policies in different ways before apply them to the several units.
For example you may use profiles when you have the following policies phone book and a couple of
ringtone policies and you need to set same phone book but different ringtones for two different
departments.


## cURL examples

### Summary of available profiles

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/profiles

### Add a profile.

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/profiles -d '{"name": "profile name", "policies":["policy_1_id", "policy_2_id", ...]}'

"config" object contains the configuration settings same as for Units or Account provision API.

### Get a specific profile's information

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/policies/{PROFILE_ID}

### Update a profile's definition

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/policies/{PROFILE_ID} -d '{"name": "profile name", "policies":["policy_1_id", "policy_2_id", ...]}'

### Delete a profile

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/policies/{PROFILE_ID}
