/* Actions prefixed with /server denote actions which trigger the sending of a websocket msg to server*/

/* User API Types */
export const AUTH_REQUESTED = 'AUTH_REQUESTED';
export const AUTHENTICATED = 'AUTHENTICATED';
export const UNAUTHENTICATED = 'UNAUTHENTICATED';
export const AUTHENTICATION_ERROR = 'AUTHENTICATION_ERROR';

/* Websocket Action Types */
export const CONNECT_SOCKET = 'CONNECT_SOCKET'
export const SOCKET_CONNECTED = 'SOCKET_CONNECTED'
export const DISCONNECT_SOCKET = 'DISCONNECT_SOCKET'
export const SOCKET_AUTH_SUCCESS = 'SOCKET_AUTH_SUCCESS'
export const SOCKET_AUTH_ERR = 'SOCKET_AUTH_ERR'
export const SOCKET_CONN_ERR = 'SOCKET_CONN_ERR'

/* Lobby Action Types */
export const GET_LOBBY = 'server/GET_LOBBY'
export const TAKE_SEAT = 'server/TAKE_SEAT'
export const NEW_LOBBY = 'NEW_LOBBY'
