#!/bin/bash
# This script resets the admin password to 'admin123'
# The hash is pre-computed using bcrypt with cost 12

HASH='$2a$12$PX552aMuizz/tKjW27Xa3.8GrrYj/PjH5KrQY1.iCmOxi9M7h.Vci'

docker exec -i riva_elixir-postgres-1 psql -U postgres -d riva_ash_dev << EOF
  UPDATE users 
  SET hashed_password = '${HASH}'
  WHERE email = 'admin@example.com';
  
  SELECT email, role FROM users WHERE email = 'admin@example.com';
EOF

echo "Admin password has been reset to: admin123"
