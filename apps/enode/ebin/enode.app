{application,enode,
             [{description,"Sends emails"},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib,lager]},
              {mod,{enode_app,[]}},
              {env,[]},
              {modules,[email_dtl,enode,enode_app,enode_sup,enode_worker]}]}.
