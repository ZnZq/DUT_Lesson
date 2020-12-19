{application, database, 
   [{description, "My Database"},
    {vsn, "1.0.0"},
    {modules, [database, database_gen, database_sup, database_app]},
    {registered, [database, database_sup]},
    {applications, [kernel, stdlib]},
    {env, []},
    {mod, {database_app,[]}}]}.
