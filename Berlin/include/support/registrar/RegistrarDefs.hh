#ifndef __REGISTRARDEFS_HH__
#define __REGISTRARDEFS_HH__ 1

#define REGISTRAR_VERSION_NAME		"0.0.2"
#define REGISTRAR_VERSION_CODE		0x00000002
#define REGISTRAR_TREE_MAGIC		0xFEEDFACE
#define REGISTRAR_DATA_MAGIC		0xF00DCAFE
#define REGISTRAR_PID			"/var/tmp/Registrar.pid"
#define REGISTRAR_RT_PATH		"/tmp/user.rt"
#define REGISTRAR_RD_PATH		"/tmp/user.rd"

#define REGISTRAR_ROOT_OFFSET		1025

#define REG_TYPE_NULL			0	/* no value */
#define REG_TYPE_STRING			1	/* null terminated string */
#define REG_TYPE_INTEGER		2	/* long int */
#define REG_TYPE_BOOLEAN		3	/* short int */
#define REG_TYPE_BINARY			4	/* binary data */
#define REG_TYPE_LINK			5	/* data points to key */

#endif /* __REGISTRARDEFS_HH__ */
