from gen3 import auth
from gen3 import submission

endpoint = "https://nci-crdc-demo.datacommons.io"
auth_obj = auth.Gen3Auth(endpoint, refresh_file="credentials.json")
print(auth_obj)

def get_auth_value():
    return auth_obj._get_auth_value()
