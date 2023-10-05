devtools::install_github('JGCRI/rgcam', build_vignettes=TRUE, force =TRUE)


conn <- localDBConn('E:\gcam-v7.0-Windows-Release-Package\output', 'my-gcamdb_basexdb')


conn

prj <- addScenario(conn, 'my-project-name.dat', 'my-scenario-name'
                   'my-batch-queries.xml')