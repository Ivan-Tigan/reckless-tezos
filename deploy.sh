set -x
admin_address="tz1Q3v9gjkG8gBHVDtG1aihop3dUVpEkvyUG"
compiled=$(./ligo compile contract $1)
storage=$(./ligo compile storage $2 $3)
tezos-client originate contract $1 transferring 0 from $admin_address running "$compiled" --init "$storage" --burn-cap 0.59525 --force
