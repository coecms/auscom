/* RMNLIB - Library of useful routines for C and FORTRAN programming
 * Copyright (C) 1975-2000  Division de Recherche en Prevision Numerique
 *                          Environnement Canada
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h> 
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <time.h>
#include <signal.h>

#include "gossip.h"

#include <fcntl.h>
#include <dirent.h>
#include <sys/dir.h>
#include <sys/param.h>

static int endian_flag = 1;
static char *little_endian = (char *)&endian_flag;
static int must_init_signal = 1;

/* size of socket buffers in KiloBytes */
#define SOCK_BUF_SIZE 256
#define BPATH         1024

#define ONE_BYTE    1
#define FOUR_BYTES  4
#define EIGHT_BYTES 8
#define IS_OK       0
#define NOT_OK      1
#define LOAD        -2

#define FALSE       0
#define TRUE        !FALSE

/* GetHostName special gethostname with IBM p690 name substitution */
int GetHostName(char *name, size_t len)  /*   %ENTRY%   */
{
  int junk;

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::GetHostName(), Host Name: %s\n", name);
#endif
  junk = gethostname(name, len);
  if( name[0] == 'c' && name[2] == 'f' && name[5] == 'p' && name[7] == 'm' && name[8] == '\0' )
    name[7] = 's';  /* name = cxfyypzm, return cxfyypzs instead */
  return(junk);
}

/* write hostname:port_number into channel description file */
/* hostname normally in IPV4 notation xx.yy.zz.tt           */
int set_host_and_port(char *channel_file, char *host_and_port)  /*   %ENTRY%   */
{
     int fserver;
     char buf[1024];
     int nc;

     if(strncmp(channel_file, "Anonym", 6) == 0) 
       return(0) ; /* anonymous channel created */

     /* $HOME/.broker/channel_name is file name for channel descriptor */
     snprintf(buf, 1023,"%s/.broker/%s", getenv("HOME"), channel_file);

     if((fserver = open(buf, O_WRONLY + O_CREAT, 0700)) == -1) 
       { 
         fprintf(stderr, "Can't open or create Channel Description file\n");
         return(-1);
       };

     nc = snprintf(buf, 1023, "%s\n", host_and_port);

     if(write(fserver, buf, nc) <= 0) 
       {
         fprintf(stderr, "Can't write into Channel Description file\n");
         return(-1);
       }
     close(fserver);
     return(0);
}

/* read hostname:port_number from channel description file 
   return pointer to character string upon success
   return NULL pointer in case of failure
*/
char *get_host_and_port(char *channel_file)  /*   %ENTRY%   */
{
     char *chan_buf = malloc(1024);
     int fd;
     char buf[1024];

#ifdef DEBUG
     fprintf(stderr, "gossip_sock::get_host_and_port(char *channel_file), channel_file = %s\n", channel_file);
#endif

     snprintf(buf, 1023, "%s/.broker/%s", getenv("HOME"), channel_file);

#ifdef DEBUG
     fprintf(stderr, "gossip_sock::get_host_and_port(char *channel_file), buf contient = %s\n", buf);
#endif

     if((fd = open(buf, O_RDONLY)) == -1) 
       {
         fprintf(stderr, "Can't open Channel Description file\n");
         return(NULL) ; /* exit(1); */
     };

     if(read(fd, chan_buf, 1024) <= 0)
       {
         fprintf(stderr, "Can't read Channel Description file\n");
         close(fd);
         return(NULL) ; /* exit(1); */
       }

     close(fd);

     if(index(chan_buf, '\n'))
       {
	 *index(chan_buf,'\n') = '\0' ;
       } 
     else
       {
         fprintf(stderr, "Invalid Channel Description file\n");
         return(NULL) ; /* exit(1); */
       }
     return(chan_buf);
}

/* get Authorization token from file $HOME/.broker/.Bauth  
   return pointer to character string upon success
   return NULL pointer in case of error
*/
char *get_broker_Authorization()  /*   %ENTRY%   */
{
     char *auth_buf = malloc(1024);
     int fd;
     char buf[1024];
     char *homedir = getenv("HOME");

     snprintf(buf, 1023, "%s/.broker", homedir);
     if(chmod(buf, 0711))
       {
         fprintf(stderr, "Improper permissions for broker directory %s\n", buf);
       return(NULL); /* exit(1); */
     }

     snprintf(buf, 1023, "%s/.broker/.Bauth", getenv("HOME"));

     if(chmod(buf, 0600))
       {
         fprintf(stderr, "Improper permissions for Authorization file\n");
         return(NULL); /* exit(1); */
       }
     if((fd = open(buf, O_RDONLY)) == -1) 
       {
         fprintf(stderr, "Can't open Authorization file\n");
         return(NULL); /* exit(1); */
       };
     if(read(fd, auth_buf, 1024) <= 0)
       {
         fprintf(stderr, "Can't read Authorization file\n");
         close(fd);
         return(NULL); /* exit(1); */
       }
     close(fd);
     if(index(auth_buf, '\n'))
       {
	 *index(auth_buf, '\n') = '\0' ;
       }
     else 
       {
         fprintf(stderr, "Invalid Authorization file\n");
         return(NULL); /* exit(1); */
     }
     return(auth_buf);
}

/* write Authorization token into file $HOME/.broker/.Bauth */
void set_broker_Authorization(int auth_token)  /*   %ENTRY%   */
{
     int fd;
     char buf[1024];
     int nc;

     snprintf(buf, 1023, "%s/.broker/.Bauth", getenv("HOME"));
     if((fd = open(buf, O_WRONLY)) == -1)
       {
         fprintf(stderr,"Can't open Authorization file\n");
         exit(1);
       };
     nc = snprintf(buf, 1023, "%d\n", auth_token);
     write(fd, buf, nc + 1);
     close(fd);
}

static struct  sockaddr_in server;                /* server socket */
static socklen_t sizeserver = sizeof server;

/* accept connections on the bound server socket return socket for incoming connection */
/* bind_sock_to_port must have been called before connection can be accepted           */
int accept_from_sock(int fserver)  /*   %ENTRY%   */
{
    int fclient =  accept(fserver, (struct  sockaddr *)&server, &sizeserver);
    if(fclient < 0) 
      {
        fprintf(stderr, "Accept failed!\n");
        return(-1);
      }
    return fclient;
}

/* bind an existing socket to a free (automatic) port, return port number */
/* existing socket usually created by get_sock_net                        */
int bind_sock_to_port(int s)  /*   %ENTRY%   */
{
     struct sockaddr_in server_eff;
     socklen_t sizeserver_eff = sizeof server_eff ;

     server.sin_family = AF_INET;
     server.sin_port = htons(0);
     server.sin_addr.s_addr = INADDR_ANY;

     if(bind(s, (struct  sockaddr *)&server, sizeserver) < 0)
          {
            fprintf(stderr, "Bind failed! \n");
            return(-1);
          }
     getsockname(s, (struct  sockaddr *)&server_eff, &sizeserver_eff);
     return ntohs(server_eff.sin_port);
}

/* create a network socket ; return socket descriptor */
int get_sock_net()  /*   %ENTRY%   */
{
/*   ignore SIGPIPE signal (i.e. do no abort but return error) */

     if(must_init_signal)
       {  /* DO THIS ONLY ONCE */
         
         signal(SIGPIPE, SIG_IGN);
         must_init_signal = 0;

       }

     return socket(AF_INET, SOCK_STREAM, 0);
}

/* set buffer sizes (recv and send) for a newly created socket (always returns 0) */
int set_sock_opt(int s)  /*   %ENTRY%   */
{
  socklen_t optval, optsize;
  int b0 = 0;
  
  optval = SOCK_BUF_SIZE*1024;
  /*      b0=setsockopt(s,SOL_SOCKET,SO_SNDBUF,(char *)&optval,sizeof(optval));*/
  b0 = setsockopt(s, SOL_SOCKET, SO_SNDBUF,(char *)&optval, sizeof(optval));

  if(b0 != 0)
    { 
      fprintf(stderr, "Error setting SO_SNDBUF size \n"); 
    }

  optval = 0;
  optsize = 4;
  getsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&optval, &optsize);

#ifdef DEBUG
  fprintf(stderr,"SO_SNDBUF=%d, optsize = %d\n", optval, optsize);
#endif

  optval = SOCK_BUF_SIZE*1024;
  b0 = setsockopt(s, SOL_SOCKET, SO_RCVBUF, (char *)&optval, sizeof(optval));

  if(b0 != 0)
    { 
      fprintf(stderr, "Error setting SO_RCVBUF size \n");
    }

  optval = 0;
  optsize = 4;
  getsockopt(s, SOL_SOCKET, SO_RCVBUF, (char *)&optval, &optsize);

#ifdef DEBUG
  fprintf(stderr, "SO_RCVBUF = %d, optsize = %d\n", optval, optsize);
#endif
  
  return(0);
}

/* obtain the IPV4 adress of a host specified by name */
int get_ip_address(char *hostname)  /*   %ENTRY%   */
{
     int **addr_list;
     struct hostent *answer;
     int ipaddr = 0;
     int b0, b1, b2, b3;

     if(NULL == (answer = gethostbyname(hostname)))
       {
         fprintf(stderr, "Cannot get address for host=%s\n", hostname);
         return(-1);
       }

     addr_list = (int **)answer->h_addr_list;
     ipaddr = ntohl(**addr_list);

     b0 = ipaddr >> 24; b1 = ipaddr >> 16 ; b2 = ipaddr >> 8 ; b3 = ipaddr;
     b0 &= 255;
     b1 &= 255;
     b2 &= 255;
     b3 &= 255;

#ifdef DEBUG
     fprintf(stderr, "IP address of %s =%d.%d.%d.%d \n", hostname, b0, b1, b2, b3);
#endif
     return(ipaddr);
}

/* obtain own host's IPV4 address 
*/
int get_own_ip_address()  /*   %ENTRY%   */
{
     char buf[1024];
     if(GetHostName(buf, sizeof buf))
       {
         fprintf(stderr, "Can't find hostname\n");
         return(-1);
       }
     return get_ip_address(buf);
}

/* given a [host:]port specification, connect to it
   if host: is not specified, use localhost
   the return value is the connected socket
*/
int connect_to_hostport(char *target2)  /*   %ENTRY%   */
{
     char buf[1024];
     int fserver;
     int b0, b1, b2, b3, sizeserver;
     int ipaddr;
     char *portno;
     char *target;

     

     struct sockaddr_in server;
     sizeserver = sizeof server;
     target = target2;

     if(NULL == strstr(target, ":"))
       {      /* no host specified, use local host */
         portno = target;
       if(GetHostName(buf, sizeof buf))
         {
           fprintf(stderr, "Can't find hostname\n");
           return(-1);
         } 
       ipaddr = get_ip_address(buf);
     }
     else 
       {  /* use specified host, find address */
         portno = strstr(target, ":"); 
         *portno = '\0';
         portno++;
         ipaddr = get_ip_address(target);
     }

#ifdef DEBUG
     fprintf(stderr, "gossip_sock::connect_to_hostport() with ipaddr: %d\n", ipaddr);
#endif

     b0 = ipaddr >> 24; b1 = ipaddr >> 16 ; b2 = ipaddr >> 8 ; b3 = ipaddr;
     b0 &= 255;
     b1 &= 255;
     b2 &= 255;
     b3 &= 255;
     snprintf(buf, 1023, "%d.%d.%d.%d", b0, b1, b2, b3);

#ifdef DEBUG
     fprintf(stderr, "Connecting to %d.%d.%d.%d:%s\n", b0, b1, b2, b3, portno);
#endif

     fserver = socket(AF_INET, SOCK_STREAM, 0);

     set_sock_opt(fserver);

     server.sin_family = AF_INET;
     server.sin_port = htons(atoi(portno));
     server.sin_addr.s_addr = inet_addr(buf);

     if(connect(fserver, (struct  sockaddr *)&server, sizeserver) < 0) 
          {
            fprintf(stderr,"Connection to Server Failed, the server may be down! \n");
            return(-1);
          }

     return(fserver);
}

/* connect to a port on local host return socket descriptor
*/
int connect_to_localport(int port)  /*   %ENTRY%   */
{
     struct sockaddr_in server;
     int sizeserver = sizeof server;
     int fserver, test;
     
#ifdef DEBUG
     fprintf(stderr, "gossip_sock::connect_to_localport(int port), server port = %d\n", port);
#endif
     
     fserver = socket(AF_INET, SOCK_STREAM, 0);
     server.sin_family = AF_INET;
     server.sin_port = htons(port);
     server.sin_addr.s_addr = INADDR_ANY;

     
#ifdef DEBUG
     fprintf(stderr, "gossip_sock::connect_to_localport(int port), fserver = %d\n", fserver);
#endif
      
      if(connect(fserver, (struct  sockaddr *)&server, sizeserver) < 0)
        {
          fprintf(stderr, "Connection to local port <%d> failed! \n", port);
          return(-1);
        }

     return(fserver);
}

/* bind a server port to a local port, return hostname:port string 
   as well as socket descriptor. bind to any free port
*/
int bind_to_localport(int *port, char *buf, int maxbuf)  /*   %ENTRY%   */
{
     int fserver, ipaddr, b0, b1, b2, b3, server_port;

/*   get a socket */
     fserver = get_sock_net();

/*   set buffer sizes for socket */
     set_sock_opt(fserver);

/*   bind to a free port, get port number */
     server_port = bind_sock_to_port(fserver);
     *port = server_port;

/*   write host:port into buffer and return the socket file descriptor */
     ipaddr = get_own_ip_address();                    /* get own IPV4 address as 32 bit integer */
     b0 = ipaddr >> 24; b1 = ipaddr >> 16 ; b2 = ipaddr >> 8 ; b3 = ipaddr;
     b0 &= 255; b1 &= 255; b2 &= 255; b3 &= 255;     /* split IPV4 address */
     snprintf(buf, maxbuf, "%d.%d.%d.%d:%d", b0, b1, b2, b3, server_port);

     return(fserver);
}

/* send reply to command, ACK if status=0, NACK if status nonzero */
void send_ack_nack(int fclient, int status)  /*   %ENTRY%   */
{
   if(status)
     write(fclient, "NACK\0", 5);
   else
     write(fclient, "ACK\0\0", 5);
}

/* get reply to command from server 0=ACK, nonzero=NACK*/
int get_ack_nack(int fserver)  /*   %ENTRY%   */
{
   char reply[5];
   int n;
   
   n = read(fserver, reply, sizeof(reply));

#ifdef DEBUG
   fprintf(stderr, "gossip_sock::get_ack_nack(): n = read() = %d\n", n);
#endif

   if (n < 3) 
     {
       fprintf(stderr, "get_ack_nack : bad reply length for ACK/NACK (5 expected) = %d\n", n);
       return(1) ; /* NACK */
     }

   return(strncmp(reply, "ACK", 3));
}

/* send command and return 0 or -1 upon ACK or NACK */
int send_command_to_server(int fserver, char *buf)  /*   %ENTRY%   */
{
  int length;
  /* send command to server */
   length = write(fserver, buf, strlen(buf));
   /* fprintf(stderr, "gossip_sock::send_command_to_server(fserver), la commande envoyee:: %s\n", buf); */
#ifdef DEBUG
   fprintf(stderr, "gossip_sock::send_command_to_server(fserver), la commande envoyee:: %s\n", buf); 
#endif

   /* and check reply */
   if(get_ack_nack(fserver)) 
     { 
       fprintf(stderr, "Command rejected,\n"); 
       return(-1);
     }
   else 
     { 
       return(0);
     }

}

/* get/put a 32 bit integer through a socket in network (BIG-ENDIAN) order */
/* can also be used for a float as long as sizeof(float) is equal */
/* to sizeof(int) and both are equal to 4                         */
INT_32 get_int32_from_channel(int channel)  /*   %ENTRY%   */
{
  INT_32 to_receive;

  read(channel, &to_receive, sizeof(to_receive));

  if(*little_endian) 
    {
      swap_4(to_receive);
    }
  return(to_receive);
}

void put_int32_to_channel(int channel, INT_32 to_send)  /*   %ENTRY%   */
{
  INT_32 to_be_sent = to_send;

  if(*little_endian)
    {
      swap_4(to_be_sent);
    }

  write(channel, &to_be_sent, sizeof(to_be_sent)); /*   %ENTRY%   */
}


static send_request(int channel, char *request)
{
  int n;
#ifdef DEBUG
  fprintf(stderr,"gossip_sock::send_request(int channel, char *request), request is: %s\n", request);
#endif
  n = write_stream(channel, request, strlen(request));
   
}

static get_request(int channel, char *request)
{
  char reply[128];
  int n;
  int len = strlen(request);
  
  len = len > sizeof(reply)-1 ? sizeof(reply)-1 : len ;
  n = read_stream(channel, reply, len);
  reply[n > 0 ? n : 0] = '\0';
  
#ifdef DEBUG
  fprintf(stderr, "gossip_sock::get_send_request(), nbre of bytes read =>: %d\n", n);
  fprintf(stderr, "gossip_sock::get_send_request(), reply =>: %s\n", reply);
#endif
  
  if(strncmp(reply, request, strlen(request)) == 0)
    return (1);
  else
    return (0);
  
}

#ifdef DEBUG
void display_records(char *buf, int size, int tokensize)
{
  int i;
  char *record;

  fprintf(stderr,"gossip_sock::display_records(), begining, size = %d\n", size);

  if(tokensize == FOUR_BYTES || tokensize == EIGHT_BYTES)
    { 
      for(i = 0; i<size; i++)
        {
          memcpy(&record, buf, sizeof(int));
          buf += sizeof(int);
          /* fprintf(stderr,"gossip_sock::display_records(), record[%d] = %.2f\n", i, swap_element(record, sizeof(int) )); */
          
          if(i == 0 || i == size - 1 )
            fprintf(stderr,"gossip_sock::display_records(), record[%d] = %.2f\n", i, ((float *)buf)[i]);
          /* fprintf(stderr,"gossip_sock::display_records(), record[%d] = %d\n", i, record); */
        }
    }
  else if(tokensize == ONE_BYTE)
    {
      fprintf(stderr,"gossip_sock::display_records(), buffer contient = %s\n",  buf);
    }
}
#endif

/* from connect_to_subchannel_by_name.c*/
int connect_to_subchannel_by_name(char *channel, char *subchannel, char *mode) /*   %ENTRY%   */
{
  /* */
  int fserver, i, current_channel, msg;
  char command[1024];

   /* login to server, get socket descriptor */
  fserver = connect_to_channel_by_name(channel);

#ifdef DEBUG
  fprintf(stderr, "fserver : %d\n" , fserver);
#endif

  if (fserver < 0) 
    return fserver;
  

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::connect_to_subchannel_by_name(), mode = %s, subchannel = %s \n", mode, subchannel);
#endif

  /* send EXEC, mode, and subchannel command to server */
  snprintf(command, sizeof(command) - 1, "EXEC %s %s", mode, subchannel);
  
  if ( send_command_to_server(fserver, command) != 0 ) return(-1);
  
  return fserver;
}

#ifdef DEBUG
int GET_ack_nack(int socket, char *message) /*   %ENTRY%   */
{
  static int compteur = 0;
  int msg;
  compteur++;

  if (message != NULL)
    {
      fprintf(stderr, "connect_to_subchannel_by_name::GET_ack_nack(): compteur: %d, socket: %d, message: %s, de longueur = %d\n", compteur, socket, message, strlen(message));
    }

  msg = get_ack_nack(socket);

  if (msg != 0)
    {
      fprintf(stderr, "connect_to_subchannel_by_name::Probleme avec le socket: %d , message: %s, reponse = %d != 0\n", socket, message, msg);
    }
  return msg;
}
#endif

/* from connect_to_subchannel_by_name.c*/

/* connect to channel_name and send LOGIN sequence */
/*         channel name has 2 forms :              */
/*                               1- name           */
/*                               2- @hostname:port */
/* login sequence has the following syntax         */
/* LOGIN uid pid Auth_token hostname               */
int connect_to_channel_by_name_2(char *name, char * msg)  /*   %ENTRY%   */
{
     char *Auth_token = get_broker_Authorization();
     int Bauth_token = 0xFFFFFFFF;
     char buf[1024];
     char host_name[1024];
     int fserver;

     if(Auth_token == NULL) 
       {
         fprintf(stderr, "Authorizartion token failure \n");
         return(-1);
       }
     if(GetHostName(host_name, sizeof host_name))
       {
         fprintf(stderr, "Can't get local hostname\n");
         return(-1);
       }

    /* connect to data server by channel_name or @host:port */
     if ( *name == '@' ) 
       {
         name++;
         fprintf(stderr, "Connecting to %s\n", name);
         fserver = connect_to_hostport(name);
         if(fserver < 0)
           return(-1);
       } 
     else 
       {
         char *host_and_port = get_host_and_port(name);
         
         if(host_and_port == NULL) 
           return(-1);

#ifdef DEBUG         
         fprintf(stderr, "Opening channel %s to name: %s and port: %s\n", name, name, host_and_port);
#endif
         fserver = connect_to_hostport(host_and_port);
	 if(host_and_port != NULL)
	   free(host_and_port);
         
#ifdef DEBUG
         fprintf(stderr, "gossip_sock:::connect_to_channel_by_name_2(), fserver = %d\n", fserver);
#endif
         if(fserver < 0) 
           {
#ifdef DEBUG
             fprintf(stderr, "gossip_sock:::connect_to_channel_by_name_2(), fserver = %d\n", fserver);
#endif
             return(-1);
           }
       }

     /*   send LOGIN command to server */
     sscanf(Auth_token, "%u", &Bauth_token);

     if(Auth_token != NULL)
       free(Auth_token);

     /* add the new info smg to the buffer content before send it to the server */
     snprintf(buf, 1023, "%s %d %d %u:%s:%s", "LOGIN", getuid(), getpid(), Bauth_token, host_name, msg);

     if(send_command_to_server(fserver, buf)) 
       {
         fprintf(stderr, "LOGIN rejected\n");
         return(-1);
       } 
     else 
       {
         fprintf(stderr, "LOGIN accepted\n");
         return(fserver);
       }
}

/* connect to channel_name and send LOGIN sequence */
/*         channel name has 2 forms :              */
/*                               1- name           */
/*                               2- @hostname:port */
/* login sequence has the following syntax         */
/* LOGIN uid pid Auth_token hostname               */
int connect_to_channel_by_name(char *name)  /*   %ENTRY%   */
{
  return connect_to_channel_by_name_2(name, "");
}

/* dummy client, black hole */
static int black_hole(int client_uid, int client_auth, int from_client, int to_client, char * buf)
{
  int buflen;
  char iobuf[65536]; int recvd; int to_receive;
  int nbuf = 0;
  
  fprintf(stderr, "Black Hole called with command:%s\n", buf);
  to_receive = get_int32_from_channel(from_client);
  fprintf(stderr, "Starting receive for %d bytes\n", to_receive);
  recvd = 0;

  while((buflen = read(from_client, iobuf, sizeof iobuf)) > 0 ) 
    {
      nbuf++;
      recvd += buflen;
      reset_timeout_counter();

      if(recvd >= to_receive)
        break;
    }
  fprintf(stderr, "server received from client %d chars in %d buffers \n", recvd, nbuf);
  return(0);
}

/* test function */
/* issue a FLOOD command and send n junk bytes to server */
/* used for test purposes only */
void send_junk_to_server(int fserver, int n)
{
  char buf[4096*1024];
  int nsent;
  int nbuf = 0;
  int nn = n > 0 ? n : -n;
  
  sprintf(buf, "%s %d", n>0 ? "FLOOD" : "STUFF" , nn);
  fprintf(stdout, buf, "%s %d", n>0 ? "FLOOD" : "STUFF", nn);
  
  if(send_command_to_server(fserver, buf)) 
    {
      fprintf(stderr, "%s rejected\n", n>0 ? "FLOOD" : "STUFF");
      close(fserver);
      exit(1);
    }
  
  fprintf(stderr, "Sending %d bytes \n", nn);
  put_int32_to_channel(fserver, nn);
  while(nn > 0)
    {
      nsent = write(fserver, buf, sizeof(buf) > nn?nn:sizeof(buf));
      nbuf++;
      if(nsent <= 0) 
        {
          fprintf(stderr, "send_junk_to_server: ERROR, write returned %d\n", nsent);
          break;
        }
      nn = nn-nsent;
    }
  fprintf(stderr, "send_junk_to_server: buffers sent = %d\n", nbuf);
  nsent = get_ack_nack(fserver);
  fprintf(stderr, "send_junk_to_server: ACK = %d\n", nsent);
}

/* ---------------------------------------------------- 
 *  Write "n" bytes to a stream socket return 0 if OK
 *  return -number of bytes not written if not OK
 * ---------------------------------------------------- */
 int write_stream(int fd, char *ptr, int  n)  /*   %ENTRY%   */
{
  int  res;
  fd_set wfds;
  struct timeval tv;
#ifdef DEBUG    
  fprintf(stderr, "gossip_sock::write_stream(), nombre de bytes a envoyer = %d\n", n);
  fflush(stderr);
#endif

  FD_ZERO(&wfds);
  FD_SET(fd, &wfds);  
  while (n > 0) 
    {
      tv.tv_sec = 5;
      tv.tv_usec = 0;
      if (select(fd+1, NULL, &wfds, NULL, &tv))
        res = write(fd, ptr, n);
      else
        return(-n);
       
      if (res <= 0) 
        return (-n);          
      n -= res;
      ptr += res;
    }

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::write_stream(), nombre de bytes envoyes = %d\n", res);
  fflush(stderr);
#endif

  return n;
} 


/* ---------------------------------------------------- 
 *   Read "n" bytes from a stream socket
 * ---------------------------------------------------- */
int read_stream(int fd, char *ptr, int nbytes)  /*   %ENTRY%   */
{
  int  n, res, bytes_read; 
  fd_set rfds;
  struct timeval tv;

  
  n = nbytes;
  bytes_read = 0;

#ifdef DEBUG 
  fprintf(stderr, "gossip_sock::read_stream(), bytes to be read = %d\n", n);
  fprintf(stderr, "gossip_sock::read_stream(), fd = %d\n", fd);
#endif
  FD_ZERO(&rfds);
  FD_SET(fd, &rfds);
  while (n > 0)
    {  
      tv.tv_sec = 5;
      tv.tv_usec = 0;
      if (select(fd+1, &rfds, NULL, NULL, &tv))
        res = read(fd, ptr, n);
      else
	{
	  return(0);
	}
      if (res <= 0)
        {
          return (res);
        }
      
      n -= res;
      ptr += res;
      bytes_read += res;
    }
#ifdef DEBUG 
  fprintf(stderr, "gossip_sock::read_stream(), after while(), bytes read = %d\n", res);
#endif   
  
  return bytes_read;
} 

/* swap elements of size tokensize bytes if little endian */
void check_swap_records(void *record, int size, int tokensize) /*   %ENTRY%   */
{
  /* if(*little_endian) */
  /* fprintf(stderr, "Machine is little endian\n"); */
  
  if(*little_endian || tokensize == ONE_BYTE)
    return;
   
  if(tokensize == FOUR_BYTES)
    {
      int i;
      
      INT_32 *element = (INT_32 *)record;

#ifdef DEBUG
      fprintf(stderr, "gossip_sock::check_swap_records(),  FOUR_BYTES\n");
#endif

      for(i = 0; i<size; i++)
        {
          /* *element = swap_4(*element); */
          swap_4(*element);
          element++;
        }
    }
  else if(tokensize == EIGHT_BYTES)
    {
      int i;
      INT_64 *element = (INT_64 *)record;
      
      for(i = 0; i<size; i++)
        {
          swap_8(*element);
          element++;
        }

#ifdef DEBUG
      fprintf(stderr, "gossip_sock::check_swap_records(),  EIGHT_BYTES\n");
#endif
    }
#ifdef DEBUG
  fprintf(stderr, "gossip_sock::check_swap_records(),  end\n");
#endif
}



/* -------------------------------------------------------------------- 
 *   Write a record to socket in the format: lentgth + record + length
 * --------------------------------------------------------------------*/
int write_record(int fclient, void *record, int size, int tokensize)  /*   %ENTRY%   */
{
  int msg, nbytes, i;
  char *rec;
  
  if(!get_request(fclient, "SEND"))
    {
      fprintf(stderr, "gossip_sock::write_record(), problem getting SEND request, using fclient = %d \n", fclient);
      return size;
    }
  
  put_int32_to_channel(fclient, size * tokensize); /* send the 1st length tag = size */
  
  check_swap_records(record, size, tokensize); /* check for data swaping */
  
  nbytes = write_stream(fclient, record, size * tokensize);
  
  
#ifdef DEBUG 
  fprintf(stderr, "gossip_sock::write_record(),  nombre de bytes non envoyes = %d\n", nbytes);
#endif
  
  check_swap_records(record, size, tokensize); /* swap back if necessary */
  
  put_int32_to_channel(fclient, size * tokensize); /* send the 2nd length tag = size */
  
  msg = get_ack_nack(fclient);
  
#ifdef DEBUG
  fprintf(stderr, "gossip_sock::write_record(),  ACK = %d\n", msg);
  
  for(i = 0; i < size; i++)
    {
      fprintf(stderr, "gossip_sock::write_records(), rec[%d] = %.1f\n", i, ((float *)record)[i]);
      
    }
#endif
  return nbytes;
}

static int swap_element(int nbr, int tokensize)
{
  if(*little_endian)
    {
      if(tokensize == FOUR_BYTES)
        swap_4(nbr);
      
      if(tokensize == EIGHT_BYTES)
        swap_8(nbr);
      
    }
  
  return nbr;
}

/* blind data sink, swallow nbytes bytes from descriptor fd. stop if error of EOF
   return 0 if success, -number of bytes not swallowed otherwise                  */
static int swallow_data(int fd, int nbytes)
{
  int bytes_read;
  char buffer[4096];
  while (nbytes > 0)
    {
      bytes_read = read(fd, buffer, sizeof(buffer) < nbytes ? sizeof(buffer) : nbytes);
      if(bytes_read <= 0) return(-nbytes);
      nbytes -= bytes_read;
    }
  return(0);
}

/* -------------------------------------------------------------------- 
 *   Read a record from socket in the format lentgth + record + length
 *   if records==NULL allocate space for data
 *   if maxlength==0 no maximum length is specified
 *   if *length!=0 record length must be  (*length) * tokensize
 * ------------------------------------------------------------------*/ 
void *read_record(int fclient, void *records, int *length, int maxlength, int tokensize)  /*   %ENTRY%   */
{
  char *records2 = NULL;
  
  int length1, length2, length3, i;
    
  tokensize = (tokensize > 1)?tokensize:1;
  
#ifdef DEBUG 
  fprintf(stderr, "gossip_sock::read_record(), before send_request() ---- fclient = %d\n", fclient);
#endif
  
  send_request(fclient, "SEND");
  
#ifdef DEBUG
  fprintf(stderr, "gossip_sock::read_record(), after send_request() ---\n");
  fprintf(stderr, "gossip_sock::read_record(), tokensize = %d \n", tokensize);
  fprintf(stderr, "gossip_sock::read_record(), length = %d \n", *length);
#endif
  
  /* read 1st length */
  length1 = get_int32_from_channel(fclient);

#ifdef DEBUG 
  fprintf(stderr, "gossip_sock::read_record(), 1st length tag = %d \n", length1);
#endif

  if(length1 > maxlength * tokensize && maxlength > 0)
    {
      fprintf(stderr, "Problem reading records, length = %d is greater than max allowed: %d \n", length1, maxlength);
      if (swallow_data(fclient, length1) != 0) 
       {
         fprintf(stderr, "gossip_sock::read_record() : cannot get enough data \n");
       }
      send_ack_nack(fclient, NOT_OK);
      return NULL;
    }
 
    
  /* records2 = malloc(length1); */
  records2 = (records == NULL)? malloc(length1):records;

  if(records2 == NULL) 
   {
     fprintf(stderr, "read_record: cannot allocate memory for data \n");
     swallow_data(fclient, length1);
     send_ack_nack(fclient, NOT_OK);
     return NULL;
   }
  
  /* read records, and get received stream length */
  length2 = read_stream(fclient, records2, length1);

  /* read 2nd length  */
  length3 = get_int32_from_channel(fclient);
 
#ifdef DEBUG
  fprintf(stderr, "gossip_sock::read_record(), 2nd length tag = %d \n", length3);
#endif
  
  if(length1 != length2)
    {
      fprintf(stderr, "Problem when reading data: record length = %d != 1st data tag = %d \n", length2, length1);
      send_ack_nack(fclient, NOT_OK);
      return NULL;
    }
  
  if(*length > 0 && *length * tokensize != length2)
    {
      fprintf(stderr, "Problem reading requested data length %d != length2 = %d\n", *length * tokensize , length2);
      send_ack_nack(fclient, NOT_OK);
      return NULL;
    }
  
  
  /* check length values */
  if(length1 != length3)
    {
      fprintf(stderr, "Problem when reading data length tags: length1 = %d != length3 = %d \n", length1, length3);
      send_ack_nack(fclient, NOT_OK);
      return NULL;
    } 
  
  /* check swap records */
  check_swap_records(records2, length1/tokensize, tokensize);

  /********************************/
  
  send_ack_nack(fclient, IS_OK);

   /* return total number of bytes read */
#ifdef DEBUG
  if(records != NULL)
    {
      for(i = 0; i<length1/sizeof(int); i++)
        {
          if( i%100 == 0 || i == length1/sizeof(int) - 1)
            fprintf(stderr, "gossip_sock::read_record(), records2[%d] = %.2f\n", i, ((float *)records)[i]);
        }
    }
  
  
  else
    {
      fprintf(stderr, "gossip_sock::read_record(), records2 == NULL \n");
    }
#endif

   *length = length2/tokensize;

#ifdef DEBUG
   fprintf(stderr, "gossip_sock::read_record(), *length = %d\n", *length);
#endif
   return records2;
}

/******************************************************************/

int store_channel_data(char *buffer, int nbytes, char *file_name)  /*   %ENTRY%   */
{
  int fd, i;
  char buf[BPATH];
  
  /* Current_Working_dir/channel_name_gsave is the data file path to be stored */
  snprintf(buf, sizeof(buf)-1,"%s_gsave", file_name);
  

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::store_channel_data(), buf = %s\n", buf);
#endif

    if((fd = open(buf, O_WRONLY + O_CREAT, 0700)) == -1) 
    { 
      fprintf(stderr, "Can't Open or Create Channel Data file\n");
      return(-1);
    }

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::store_channel_data():  nbytes = %d\n", nbytes);
#endif

  
  if(write(fd, (char *)buffer, nbytes) != nbytes) 
    {
      fprintf(stderr, "store_channel_data: Error writing into data file\n");
      close(fd);
      return(-1);
    }
  close(fd);
  return(0);
}

int file_select_all(struct direct *entry)   /*   %ENTRY%   */
{
  if ((strcmp(entry->d_name, ".") == 0) || (strcmp(entry->d_name, "..") == 0))
    return (FALSE);
  else
    return (TRUE);
}

long fsize(FILE* fd) 
{
  long savepos, size;
  
  savepos = ftell(fd);          /* sauvegarder la position */
  fseek(fd, 0, SEEK_END);       /* aller a la fin */
  size = ftell(fd);             /* lire la taille */
  fseek(fd, savepos, SEEK_SET); /* retablir la position */
  
  return size;
}


int get_file_size(char *file_name)   /*   %ENTRY%   */
{
  FILE *ifp;
  char buf[BPATH];
  struct direct **files;
 /*  char pathname[BPATH]; */
  int the_size;
  
 /*  if (getcwd(pathname, BPATH) == NULL ) */
/*     {  */
/*       fprintf(stderr, "Error getting path \n"); */
/*       exit(0); */
/*     } */
  
#ifdef DEBUG
  /* fprintf(stderr, "Current Working Directory =$$$$$$$$$ %s \n", pathname); */
  fprintf(stderr, "gossip_sock::get_file_size: file_name = %s\n", file_name);
#endif
  /* snprintf(buf, 1023,"%s/%s_gsave", pathname, file_name); */
  snprintf(buf, 1023,"./%s_gsave", file_name);
#ifdef DEBUG  
  fprintf(stderr, "gossip_sock::get_file_size(): buf = %s\n", buf);
#endif
 
  if ((ifp = fopen(buf, "r")) == NULL)
    {
      fprintf(stderr, "data file: %s, doesn't exist!\n", buf);
      return 0;
    }
  
#ifdef DEBUG
  fprintf(stderr, "gossip_sock::get_file_size(): ifp = %d\n", ifp);
#endif
  the_size = fsize(ifp);
  fclose(ifp);

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::get_file_size(), file size = %d\n",the_size );
#endif
  return the_size;
}

int read_data_file(char *file_name, char *buffer, int size)   /*   %ENTRY%   */
{
  int c, i, fd;
  FILE *ifp;
  char buf[BPATH];
  char nbuf[BPATH];
  int count;
  
  struct direct **files;
  
  char pathname[BPATH];

#ifdef DEBUG 
  fprintf(stderr, "gossip_sock::read_data_file(): file_name = %s\n", file_name);
#endif

  snprintf(buf, sizeof(buf) - 1, "./%s_gsave", file_name);

#ifdef DEBUG 
  fprintf(stderr, "gossip_sock::read_data_file(): buf = %s\n", buf);
#endif

  i = 0;
  if((fd = open(buf, O_RDONLY)) == -1) 
    {
      fprintf(stderr, "data file: %s doesn't exist\n", buf);
      return(-1) ; /* exit(1); */
     }

#ifdef DEBUG
  fprintf(stderr, "gossip_sock::read_data_file(): fd ==== %d \n", fd);
#endif

  i = read(fd, buffer, size);

  if( i > size || i <= 0)
     {
      fprintf(stderr, "Can't read data file, i = %d, size = %d\n", i, size);
      close(fd);
      return(-1) ; 

    }

  close(fd);

#ifdef DEBUG  
  fprintf(stderr, " Will Try to rename data file < %s > ***\n", buf);
#endif
 
  snprintf(nbuf, sizeof(nbuf) - 1, "%s_gback", file_name);

#ifdef DEBUG
  fprintf(stderr, " Will Try to rename data file < %s > ***\n", nbuf);
#endif
  
  if((c = rename(buf, nbuf)) < 0)
    fprintf(stderr, "Can't rename data file\n");

#ifdef DEBUG      
  fprintf(stderr, "Data file < %s > renamed succefully, return result = %d\n", nbuf ,c);
  fprintf(stderr, "gossip_sock::read_data_file(), number of bytes read i === %d\n", i);
#endif
  fprintf(stderr, "gossip_sock::read_data_file(), number of bytes read i === %d\n", i);
  /******** return data file size ***********/
  return i;
}



/* ****************************************************************** */
/*                                                                    */
/* basic server code, forking variety ( as opposed to thread variety) */
/*                                                                    */
/* ****************************************************************** */
void gossip_fork_server(char *LOGFILE, char *channel, int (*user_client)(int, int, int, int, char *), int PING_INTERVAL, int from_inetd)   /*   %ENTRY%   */
{
    char *Auth_token;
    unsigned int Bauth_token;
    char buf[1024];
    char buf2[1024];
    int from_client, to_client, fserver;
    int ping_pid = 0;
    int client_uid, client_pid, client_auth;
    int buflen;
    int server_port = 0;
    int child_pid = 0;
    int child_status = 0;
    int myuid = getuid();

    if(from_inetd) /* inetd / xinetd mode */
      {     
        Auth_token = NULL;
        Bauth_token = 0;
        fserver = -1;
        from_client = 0;         /* stdin */
        to_client = 1;           /* stdout */
        
        buflen = read(from_client, buf, sizeof buf);    /* get LOGIN command */
        buf[buflen] = '\0';
        
        close(2);    /* close and reopen STDERR */
        freopen(LOGFILE, "a", stderr);
        
        client_uid = -1 ;
        client_pid = -1 ;
        client_auth = -1 ;
        sscanf(buf, "LOGIN %d %d %u", &client_uid, &client_pid, &client_auth);
        
        send_ack_nack(to_client,     /* send ACK/NACK according to client code */
                      (*user_client)(client_uid, client_auth, from_client, to_client, buf)  );
        close(from_client);
        close(to_client);
        exit(0);
      }
    else   /* server mode, bind to port, get server socket */
      {                   
        
        close(0); /* reopen stdin on /dev/null */
        freopen("/dev/null", "r", stdin);
        Auth_token = get_broker_Authorization();
        sscanf(Auth_token, "%u", &Bauth_token);
        
        fserver = bind_to_localport(&server_port, buf, 1023);
        if(GetHostName(buf2, sizeof buf2))
          {
            fprintf(stderr, "Can't find hostname\n");
            exit(1);
          }
      
        from_client = -1;
        to_client = from_client;
        
        printf("@%s:%d", buf2, server_port);
        fflush(stdout);
        if( set_host_and_port(channel, buf) ) exit(1) ;

        close(1);     /* close and reopen STDOUT */
        close(2);    /* close and reopen STDERR */
        freopen(LOGFILE, "a", stdout);
        freopen(LOGFILE, "a", stderr);


      if( fork() > 0 ) exit(0);   /* parent exits now if regular server */
      setpgrp();

/*    set NO BUFFERING for STDOUT and STDERR */
      setvbuf(stdout, NULL, _IONBF, 0);
      setvbuf(stderr, NULL, _IONBF, 0);

/*    the first child is the actual server */

      listen(fserver, 5);
/* ==================================================================================== */
      reset_timeout_counter();
      if(PING_INTERVAL) 
        {
          if( (ping_pid = fork()) == 0 )
            {
              int watchdog_client;
              int ping_ord = 0;
              char pingbuf[128];
              
              close(fserver);
              sleep(PING_INTERVAL);
              while(watchdog_client = connect_to_localport(server_port))
                {
                  /*send PING command with ping counter                                   */
                  sprintf(pingbuf, "PING %d", ping_ord++);
                  write(watchdog_client, pingbuf, strlen(pingbuf));
                  close(watchdog_client);
                  
                  sleep(PING_INTERVAL);
                }
              exit(0);
            } /* endif fork */
        } /* endif PING_INTERVAL */
      /* ============================================================================== */
      /* loop on connection requests */
      while( from_client = accept_from_sock(fserver) ) 
        {
          to_client = from_client;
          
          while( (child_pid = waitpid(-1, &child_status, WNOHANG)) > 0 )
          if(child_status==128) exit(0);  /* END COMMAND HAS BEEN SENT */
          fprintf(stderr, "Child pid %d terminated with status=%d\n", child_pid,child_status) ;
        
          buflen = read(from_client, buf, (sizeof buf) - 1);    /* get command */
          buf[buflen] = '\0';
        
          if(strncmp(buf, "PING", 4) == 0) 
            {                     /* PING ? */
              fprintf(stderr, "%s (%d)\n", buf, get_timeout_counter()); /* echo to logfile */
              close(from_client);

              if(get_timeout_counter() <= 0)
                {
                  fprintf(stderr,"TIMEOUT detected, EXITING \n");

                  if(ping_pid > 0) 
                    kill(ping_pid, 9);
                 
                  exit(1);
                }
              decrement_timeout_counter();
              continue;   /* go service next connection request */
            }
          /*      validate login command, check client uid against server uid,
                  verify that authorization token is the right one,
                  register client pid as connected if connection accepted      */
          
        client_uid = -1 ;
        client_pid = -1 ;
        client_auth = -1 ;
        sscanf(buf, "LOGIN %d %d %u", &client_uid, &client_pid, &client_auth);
        
        /*  reject connection if not my uid or bad authorization token or not a LOGIN command */
        if(client_uid != myuid || Bauth_token != client_auth)
          {
            fprintf(stderr, "expected client = %d, got %d, expected auth= %d, got %d\n",
                    myuid, client_uid, Bauth_token, client_auth);
            send_ack_nack(to_client, NOT_OK);  /* Negative ACK, LOGIN rejected */
            close(to_client);
            continue;
          } 
        else 
          {
            fprintf(stderr, "%s\n", buf);
            send_ack_nack(to_client, IS_OK);   /* LOGIN accepted, send ACK */
            continue;
          }
        
        /* LOGIN sequence is valid, continue processing */
        
        reset_timeout_counter();        /* reset TIMEOUT counter */
        if ( fork != 0 ){
           close(to_client);
        }else{                          /* fork server */
           while( (buflen = read(from_client, buf, sizeof buf)) > 0 ) 
           {  /* get next command */
             buf[buflen] = '\0';
             fprintf(stderr, "%s\n", buf);   /* echo command to logfile */
             if(strncmp(buf, "ECHO", 4) == 0)                                 /* ECHO command */
               {
                 send_ack_nack(to_client, IS_OK); /* send ACK to indicate that command accepted */
                 continue;
               } 
             else if(strncmp(buf, "END", 3) == 0)                              /* END command */
               {
                 send_ack_nack(to_client, IS_OK); /* send ACK to indicate that command accepted */
                 close(to_client);
                 fprintf(stderr,"END command received, server exiting normally\n");
 
                 if(ping_pid > 0) 
                   kill(ping_pid, 9);
                 
                 exit(128);
 
               } 
             else if(strncmp(buf,"EXEC",4) == 0 || strncmp(buf,"FORK",4) == 0)/* EXEC command */
               {
                send_ack_nack(to_client,        /* send ACK/NACK according to client code */
                        (*user_client)(client_uid, client_auth, from_client, to_client, buf)  );
                continue;
                
              } 
            else if( strncmp(buf, "STUFF", 5) == 0 )                         /* STUFF command */
              {
                send_ack_nack(to_client, IS_OK); /* send ACK to indicate that command accepted */
                send_ack_nack(to_client,         /* send ACK/NACK according to client code */
                            black_hole(client_uid, client_auth, from_client, to_client, buf)  );
                continue;
                
              } 
            else /* UNRECOGNIZED command, reject and close connection  */
              { /* assumed to be a bum call */
                fprintf(stderr, "Bad Command= %s\n", buf);
                send_ack_nack(to_client, NOT_OK); /* send NACK to indicate command rejection */
                continue;
              }
          } /* end while get next command */

          close(to_client); exit(0);

        } /* end of if fork() */
        
      } /* end of loop on connection requests */
      
      fprintf(stderr,"Accept failed , exiting \n");

      if(ping_pid > 0)
        kill(ping_pid, 9);

      exit(1);
    }
}
