db.host = "localhost"

# Lasting Hymns Project
lhp.con = dbConnect(MySQL(), user = "USERNAME",
                    password = "PASSWORD",
                    dbname = "lhp", host = db.host, port = 3306)
dbGetQuery(lhp.con, "SET NAMES utf8")

