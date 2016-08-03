/*
Section: Crossbar
Title: Policies
Language: en-US
*/

# Policies
The policies are phone's configuration settings what could be applied to any unit of the account.
For example policy might be a phone book settings and you could apply it to the units you want to have this phone book loaded.
The policies are used in case when you need to have some common configuration settings for more than one unit but not for all the units of the account.

## cURL examples

### Summary of available policies

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/policies

### Add a policy.

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/policies -d '{"name": "policy name", "config": {...}}'

"config" object contains the configuration settings same as for Units or Account provision API.

### Get a specific policy's information

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/policies/{POLICY_ID}

### Update a policy's definition

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/policies/{POLICY_ID} -d '{"name": "new policy name", , "config": {...}}'

### Delete a policy

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/policies/{POLICY_ID}
