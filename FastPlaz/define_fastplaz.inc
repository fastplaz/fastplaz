
{$DEFINE FASTPLAZ}

{$DEFINE short_url}
{disabled $DEFINE GREYHOUND}
{$DEFINE DEBUG}
{disabled $DEFINE SYNAPSE}

{$ifdef SYNAPSE}
  {disabled $DEFINE XMAILER}
  // OpenSSL plugin used by xmailer (Synapse); without these the 1.0.x
  // plugin ssl_openssl is used. Pick the one matching the installed library.
  {disabled $DEFINE XMAILER_OPENSSL11}
  {disabled $DEFINE XMAILER_OPENSSL3}
{$endif}
// Without XMAILER, TMailer (mailer_lib) sends through smtp_lib, the built-in
// SMTP client (FPC sockets + OpenSSL for STARTTLS/SSL), no Synapse needed.

{ $DEFINE dOPF}
{ $DEFINE zeos}

// requirement : SynEdit package
{ $DEFINE syntext}

{disable $DEFINE HEAPTRACE}


// INTEGRATION
{$DEFINE SLACK_INTEGRATION}
{$DEFINE NEWRELIC_INTEGRATION}
{$DEFINE CLOUDINARY_INTEGRATION}

