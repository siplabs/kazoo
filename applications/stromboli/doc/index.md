/*
Section: Stromboli
Title: Stromboli
Language: en-US
*/

# Overview

The Stromboli application is used for automatic provisioning of the phones connected to the Kazoo platform.
In general it does provisioning by sending configuration files to the phones using http protocol.
The configuration files are generated on the fly depending on the individual settings associated with the phone which
requests for this files.


##  Provision algorithm basics

In order to provision a phone Stromboli needs to receive the request from a phone, identify the phone and
find in a database the configuration settings which correspond to the phone, generate the configuration files
on the base of that settings and send the files back to the phone. To do this Stromboli has a special structure
in a data base called a unit each unit corresponds to some physical phone. To make the link between a phone and a unit
you need to set the mac_address field of the unit equal to the phone's mac address.

Any unit is associated with some account in other words the units are attached to the accounts.
This is made to make a configuration process more flexible. So to find a unit by mac address you first
need to determine the account the unit belongs to. In the way to allow Stromboli identify the account which
the unit corresponding the phone you want to provision belongs to, you need to following.
First set the provisioning server URL in the phone's settings equal to the account's realm and second run Stromboli
at the host this URL points to. So than Stromboli receives a request from a phone it takes an host URL from request
and than finds the account with the realm equal to this URL. After it tries to find the unit with mac_address field
equal to the phone's one and if does it starts to generate the configuration files on a base of the data
the unit contains.

##  Provision configuration API
There are four APIs to configure Stromboli we'll explain typical workflow you need to follow if
you want to provision some phones.

Different models of phones requires the different sets of configuration files in the different formats.
If the phone present in the list of supported devices than Stromboli knows what files and format the phone requires.
Configuration parameters of the phones stored in database in JSON format, you can get complete list of supported
parameters for any device Stromboli supports using API called "Unit options".
Read the APIs documentation for details.

The rest three APIs are: Accounts provision, Policies and Units are used to set configuration parameters of the phones.
All three used for same final purpose - create configuration files on the base of them.
You can choose the API fist best to your purposes.

Often we have a situation when almost all the parameters you need to configure are same for all the devices.
In that case you can use Account provision API it allows to set the parameters what would be common for all the
units of the account. Another case when that API may be used is if you want to put some restrictions on
the parameters for a children accounts. You can read the APIs documentation for details.
The example of situation when the API is used is network settings of the phone which is usually
same for all the devices. Anther example is the situation when a provider want to put the restrictions
on codecs settings.

The next common situation is when you need some settings which is common for the groups of phones but not for all.
For example you need different phone books for different departments. For such cases you can use the Profiles API.
First you create one ore more set of settings (that is called policy), for example phone books and two different
set of settings for ringtone. And than you can create a profiles which would contain same phone book, but
different ringtone settings and apply that profiles to the units which belong to the different departments
in your company.  It resembles the group policy mechanism of Microsoft Active Directory software.

The last API is the Units one. It typically used to set some individual parameters for example mac address
and also connect the Kazoo devices and the phone's lines. But you can set any parameters the phone support here.

General algorithm of configurations is following: after request from a phone received  Stromboli first finds
the unit corresponding to the phone. It reads the configuration section of the unit and also the policies list
of the unit and than applies all the profiles to the configuration of the unit. If the unit has some parameters
that matches the policy parameters, the units parameters will be redefined. If several policies have matching
parameters in the end you will have the parameters of the policy which appears in policies list last.

After that Stromboli reads the configuration of the account and apply it to the configuration it has after previous
step redefining matching parameters again. So all in all the account parameters are strongest.

After that Stromboli generate configuration file the device needs and sends it to the device.


