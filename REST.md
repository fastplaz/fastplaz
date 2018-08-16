# REST Understanding**REST (Representational State Transfer)** was introduced and defined in 2000 by Roy Fielding in his [doctoral dissertation](https://www.ics.uci.edu/%7Efielding/pubs/dissertation/top.htm). REST is an architectural style for designing distributed systems. It is not a standard but a set of constraints, such as being stateless, having a client/server relationship, and a uniform interface. REST is not strictly related to HTTP, but it is most commonly associated with it.
## Principles of REST- **Resources** expose easily understood directory structure URIs.
- **Representations** transfer JSON or XML to represent data objects and attributes.
- **Messages** use HTTP methods explicitly (for example, GET, POST, PUT, and DELETE).
- **Stateless** interactions store no client context on the server between requests. State dependencies limit and restrict scalability. The client holds session state.

## HTTP methods

Use HTTP methods to map CRUD (create, retrieve, update, delete) operations to HTTP requests.

The HTTP verbs comprise a major portion of our “uniform interface” constraint and provide us the action counterpart to the noun-based resource. The primary or most-commonly-used HTTP verbs (or methods, as they are properly called) are POST, GET, PUT, PATCH, and DELETE. These correspond to create, read, update, and delete (or CRUD) operations, respectively. There are a number of other verbs, too, but are utilized less frequently. Of those less-frequent methods, OPTIONS and HEAD are used more often than others.

| Method | CRUD | Entire Collection (e.g. /customers) | Specific Item (e.g. /customers/{id} |
|---|---|---|---|
| POST | Create | 201 (Created), 'Location' header with link to /customers/{id} containing new ID. | 404 (Not Found), 409 (Conflict) if resource already exists.. |
| GET | Read | 200 (OK), list of customers. Use pagination, sorting and filtering to navigate big lists. | 200 (OK), single customer. 404 (Not Found), if ID not found or invalid. |
| PUT | Update/Replace | 405 (Method Not Allowed), unless you want to update/replace every resource in the entire collection. | 200 (OK) or 204 (No Content). 404 (Not Found), if ID not found or invalid. |
| PATCH | Update/Modify | 405 (Method Not Allowed), unless you want to modify the collection itself. | 200 (OK) or 204 (No Content). 404 (Not Found), if ID not found or invalid. |
| DELETE | Delete | 405 (Method Not Allowed), unless you want to delete the whole collection—not often desirable. | 200 (OK). 404 (Not Found), if ID not found or invalid. |

### GET

Retrieve a customer with an ID of 1:

```
GET /customer/1
```

Retrieve a customer's orders with an ID of 1:

```
GET /customer/12345/orders
```



### POST

create new customer:

```
POST /customer
```

### PUT

Modify the address with an ID of 1:

```
PUT /customer/1
```

### PATCH

Update only the specified fields of an entity at a URI. A PATCH request is neither safe nor idempotent (RFC 5789). That's because a PATCH operation cannot ensure the entire resource has been updated.

```
PATCH /customer/1
```

### DELETE

Delete an address with an ID of 1:

```
DELETE /customer/1
```


## HTTP status codes

Status codes indicate the result of the HTTP request.

- 1XX - informational
- 2XX - success
- 3XX - redirection
- 4XX - client error
- 5XX - server error
## Media types

The `Accept` and `Content-Type` HTTP headers can be used to describe the content being sent or requested within an HTTP request. The client may set `Accept` to `application/json` if it is requesting a response in JSON. Conversely, when sending data, setting the `Content-Type` to `application/xml` tells the client that the data being sent in the request is XML.
