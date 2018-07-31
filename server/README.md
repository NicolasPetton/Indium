# Indium server

This documentation describes the version 2.0 of the server component of
[Indium](https://indium.readthedocs.org).

The Indium server exposes a common and simple interface to communicate with a JS
runtime for debugging.  It currently supports Chrome/Chromium (debug protocol
1.2), and NodeJS >= 8.

## Installation

    npm install -g indium

## Protocol

Communication is done through stdin/stdout using the JSON format.
Each message, from the server or the client, is terminated with line feed.

### 1. Overview

The client communicates with the server through JSON messages, referred to as
requests.  The server answers with corresponding JSON messages, referred to as
responses.

The response order is not guaranteed to follow the request order.  In order to
identify what request a specific response is emitted for, each request must
contain a unique `"id"` field.  Its response will also contain an `"id"` field
with the same value.

For each request, the client is guaranteed to receive at some point in time a
response.  The response can either be successful, or unsuccessful.

Responses contain a `"type"` field to identify whether they the
request completed successfully or not.  Successful responses have their `"type"`
feild set to `"success"`, and unsuccessful responses to `"error"`.

#### Requests

Requests must follow the following general structure:

id `<string>`
: Id of the request 

type `<string>`
: Type of request.  The type specifies the category of the request.  Valid types
  are `"version"`, `"connection"`, `"configurations"` and `"runtime"`.

payload `<object>`
: Payload of the request.  Contains the details specific to the request.

`payload` objects must specify an `action` key.  The `action` key determines the
action to be executed by the server.  Valid actions depend the on `type` of
request.

#### Responses

Each response contains the `id` of the corresponding request and follows the
following structure:

id `<string>`
: Id of the request the response is for

type `<string>`
: Type of response, either `"success"` or `"error"`

payload `<object>` *(optional)*
: Optional payload of the response

### 2. `configurations` requests

The server is responsible for looking up the `.indium.json` configuration file
from a project directory.

#### `list` actions

The `list` action fetches the available configurations for a directory.

**Request payload:**

action `"list"`
: Action type

directory `<string>`
: Directory where the `.indium.json` project file look up is started.

**Successful response payload:**

Array of configurations.

### 3. `connection` request type

`connection` requests are used to open/close connections to a runtime.

#### `connect` actions

**Request payload:**

action `"connect"`
: Action type

directory `<string>`
: Directory where the `.indium.json` project file look up is started.

name `<string>`
: Name of the configuration to choose for the runtime connection.

**Successful response payload:**

No payload.

#### `close` actions

**Request payload:**

action `"connect"`
: Action type

**Successful response payload:**

No payload, but the server exits.


### 4. `runtime` request type

The `runtime` request type is the most important type of request as evaluation,
inspection and debugging is done through runtime requests.

#### `evaluate` actions

Evaluation is context-sensitive.  During a debugging session when the runtime is
paused, evaluation is done in the context of the current stack frame, with full
access to all locals.

**Request payload:**

action `"evaluate"`
: Action type

expression `<string>`
: Expression to evaluate

**Successful response payload:**

A [remote object](#remote-object).

#### `getCompletion` actions

**Request payload:**
**Successful response payload:**

#### `activateBreakpoints` actions

**Request payload:**
**Successful response payload:**

#### `deactivateBreakpoints` actions

**Request payload:**
**Successful response payload:**

#### `addBreakpoint` actions

**Request payload:**
**Successful response payload:**

#### `removeBreakpoint` actions

**Request payload:**
**Successful response payload:**

#### `resume` actions

**Request payload:**
**Successful response payload:**

#### `stepInto` actions

**Request payload:**
**Successful response payload:**

#### `stepOut` actions

**Request payload:**
**Successful response payload:**

#### `stepOver` actions

**Request payload:**
**Successful response payload:**

#### `getSource` actions

**Request payload:**
**Successful response payload:**


### 5. server notifications

#### `breakpointResolved` notifications

**Request payload:**
**Successful response payload:**

#### `paused` notifications

**Request payload:**
**Successful response payload:**

#### `resumed` notifications

**Request payload:**
**Successful response payload:**

### 6. Objects in payloads

#### <a name="remote-object"></a> Remote object

### 7. Examples

In the the following example, `>>` represents stdin and `<<` represents stdout.

    > indium
	>> {"id": "1", "type": "configurations", "payload": {"action": "list", "directory": "/home/user/proj/"}}
	<< {"id": "1", "type": "success", "payload": [{"type": "chrome", "name": "Web Project", "root": "./src"}]}
    >> {"id": "2", "type": "connection", "payload": {"action": "connect", "name": "Web Project", "directory": "/home/user/proj/""}}	
	<< {"id": "2", "type": "success"}
	>> {"id": "3", "type": "runtime", "payload": {"action": "evaluate", "expression": "1+2"}}
	<< {"id": "3", "type": "success", "payload": {"description": "3"}}
    >> {"id": "4", "type": "connection", "payload": {"action": "close"}}
	> 
