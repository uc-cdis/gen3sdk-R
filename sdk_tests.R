library(httr)
library(jsonlite)
library(readr)
library(gen3sdk)


link = "https://nci-crdc-demo.datacommons.io"

auth <- Gen3AuthHelper(endpoint=link, refresh_file="credentials.json")

sub <- Gen3Submission(endpoint=link, auth_provider=auth)



########  Gen3Submission Tests  ########

#json_data <- fromJSON("create_project.json")
#response <- sub$create_project("prog1",json_data)
#print(response)
#str(content(response))

#json_data <- fromJSON("submit_record.json")
#response <- sub$submit_record("prog1", "proj1", json_data)
#print(response)
#str(content(response))

#response <- sub$export_record("prog1", "proj1", <uuid from submit_record>, "json")
#print(response)

#response <- sub$export_record("prog1", "proj1", "1ed31d69-79dd-4c86-9a1b-309c5ed26c5f", "json", "record.json")
#print(response)

#response <- sub$export_record("prog1", "proj1", <uuid from submit_record>, "tsv")
#print(response)

#response <- sub$export_record("prog1", "proj1", "1ed31d69-79dd-4c86-9a1b-309c5ed26c5f", "tsv", "record.txt")
#print(response)

#response <- sub$delete_record("prog1", "proj1", "af2b99d4-a320-4fb3-bc5f-63984cdb26ff")
#print(response)
#str(content(response))

#response <- sub$delete_project("prog1", "proj1")
#print(response)
#str(content(response))

#response <- sub$delete_program("prog1")
#print(response)
#str(content(response))

#response <- sub$export_node("prog1", "proj1", "project", "json")
#print(response)

#response <- sub$export_node("prog1", "proj1", "project", "json", "node.json")
#print(response)

#response <- sub$export_node("prog1", "proj1", "project", "tsv")
#print(response)

#response <- sub$export_node("prog1", "proj1", "project", "tsv", "node.txt")
#print(response)

#response <- sub$get_dictionary_node("program")
#print(response)
#str(content(response))

#response <- sub$get_dictionary_all()
#print(response)
#str(content(response))

#response <- sub$get_graphql_schema()
#print(response)
#str(content(response))

#query <- "{ project(first:0) { code } }"
#response <- sub$query(query)
#print(response)
#str(content(response))



########  Gen3File Tests  ########

#guid_instance <- "000003f6-a029-420a-bc84-a9777b7c34a5"
#guid_instance <- "00006cd9-863c-4ba4-bf6d-4ca2cac81fa6"
#guid_instance <- "0000a1dc-5a27-434e-9d37-ff0025f14f31"
#guid_instance <- "0000a1dc-5a27-434e-9d37-ff0025f14f31"
#guid_instance <- "0000ec5c-a1c4-4329-990d-cf58cc7758f9"
#guid_instance <- "00013934-97c9-413f-a640-3f4bd01a5e92"
#guid_instance <- "0001e22b-aba4-4046-85a4-6d2b811d74c2"
#guid_instance <- "0001e87f-4714-4607-bfd9-b4155f6cac8d"
#guid_instance <- "00022579-32f8-4ff0-9379-96c4cdfa57b8"
#guid_instance <- "00022cee-7a98-45f8-85ed-c5113a08475c"
#guid_instance <- "0000ea33-b3a5-4eb0-b75d-4858e8612989"
#response <- file$get_presigned_url(guid_instance)
#print(response)
#print(content(response))