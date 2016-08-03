/*
Section: Crossbar
Title: Account provision
Language: en-US
*/

Accounts provision is used to configure parameters which is common for all the units (look at units API for more details) of the account or all the units of subaccounts.
The API is used to get information about the units configuration associated with the account, and also create, modify and delete it.
Although you can configure each unit individually that is not the best way because most settings are same for all the units of the account, so there is no need to repeat them for all the units.
Instead you can just put them all to the configuration associated with the account and all the work would be done.
Another case account provision might be useful for is when you need to restrict some settings for the subaccounts, let's say you are a provider and you want to lock codec settings for the resellers.

# Accounts provision

## cURL examples

### Summary of accounts provision
Nothing or the only one document would be shown here, because there are only two opportunities for account configurations: it exists or not.

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/accounts_provision

### Add a configuration

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/accounts_provision -d '{ "config":{...}, "locks":[...]}'

The config "key" contains configuration itself, the "locks" would be explained later.

### Get an account provision configuration
There are two ways of getting a configuration. First you can get the configuration of the account itself.

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/accounts_provision/{CONF_ID}

Second you can get the configuration which is made by summarizing the current account configuration and the configurations of ancestor accounts.

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/accounts_provision/_hierarchical

The rules of summarizing would be explained later.

### Update an account provision configuration

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/accounts_provision/{CONF_ID} -d '{ "config":{...}, "locks":[...]}'

### Delete an account provision configuration

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/accounts_provision/{CONF_ID}

### The explanation of hierarchical summarizing rules.

Then a provisioner requests the configuration for the current account it gets the one which is sum of configuration for all the account tree.
For example if you have the following hierarchy of the accounts: root_acc -> op_acc -> reseller_acc. And the following provision configurations for the accounts.

For the root_acc:

    {
        "config":{
            "root_key":"root_value",
            "first_same_key":"root_first_same_key_value",
            "second_same_key":"root_first_same_key_value",
            "complex_key":{
                "root_complex_key":"root_complex_key_value"
            }
        },
        "locks":[]
    }

For the op_acc:

    {
        "config":{
            "first_same_key":"op_first_same_key_value",
            "second_same_key":"op_first_same_key_value",
            "op_key":"op_value",
        },
        "locks":[]
    }

For the reseller_acc:

    {
        "config":{
            "second_same_key":"reseller_first_same_key_value",
            "complex_key":{
                "reseller_complex_key":"reseller_complex_key_value"
            }
        },
        "locks":[]
    }

The configurations for the reseller_acc a provisioner will get would be:

    {
        "config":{
            "root_key":"root_value",
            "first_same_key":"op_first_same_key_value",
            "second_same_key":"reseller_first_same_key_value",
            "op_key":"op_value",
            "complex_key":{
                "root_complex_key":"root_complex_key_value",
                "reseller_complex_key":"reseller_complex_key_value"
            }
        },
        "locks":[]
    }

The rules of summarizing are: if there is same key in parent and child accounts the value of the key would be taken from child one.
If a parent's account key doesn't exist in a child account the key value pair would be added to child account.
That rules applied for all the tree consequently starting from the root, the complex keys are processing recursively.

### The locks

Sometimes you need to prevent the keys set in parent account from changing in the child accounts and you could use locks for this.
If we lock "first_same_key" in our example

    {
        "config":{
            "root_key":"root_value",
            "first_same_key":"root_first_same_key_value",
            "second_same_key":"root_first_same_key_value",
            "complex_key":{
                "root_complex_key":"root_complex_key_value"
            }
        },
        "locks":[{"first_same_key":"locked"}]
    }

and the rest stay same a provisioner would get the following configuration:

    {
        "config":{
            "root_key":"root_value",
            "first_same_key":"root_first_same_key_value",
            "second_same_key":"reseller_first_same_key_value",
            "op_key":"op_value",
            "complex_key":{
                "root_complex_key":"root_complex_key_value",
                "reseller_complex_key":"reseller_complex_key_value"
            }
        },
        "locks":[{"first_same_key":"locked"}]
    }

To lock some key you need to add an object locks array.
For example "locks":[{"first_same_key":"locked"}, {"complex_key":{"root_complex_key":"locked"}}].
As you can see in case of complex objects you need to describe all the path starting from the "config" key to the object you want to lock.
The value of the key you want to lock must be set to "locked".