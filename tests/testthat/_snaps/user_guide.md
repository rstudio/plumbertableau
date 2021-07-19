# user_guide()

    Code
      cat(render_user_guide("", pr))
    Output
      <!DOCTYPE html>
      <head>
        <meta charset="UTF-8">
        <meta http-equiv="Content-Security-Policy" content="default-src 'self'; child-src 'none';">
        <link rel="stylesheet" type="text/css" href="__plumbertableau_assets__/styles.css">
        <!-- HEAD_CONTENT -->
      </head>
      <body>
      
      <div class="container">
      
      <header>
        <h1>
          String utilities
          (v1.0.0)
        </h1>
        <div class="api-desc">
          <h3>Description</h3>
      
      <p>This is a Tableau Analytics Extension.</p>
      
      <hr/>
      
      <p>Simple functions for mutating strings</p>
      
          <p>
            <a href="./setup">Tableau Setup Instructions</a>
            <br/>
            <a href="./__docs__/">Open API Documentation</a>
          </p>
        </div>
      </header>
      <main>
        <div class="routes">
          <div class="route">
            <h3 id="/lowercase" class="path">
              /lowercase
              <a class="permalink" href="#/lowercase">#</a>
            </h3>
            <div class="desc">
              <h4>Description</h4>
              <div>Lowercase incoming text</div>
            </div>
            <div class="usage">
              <h4>Usage</h4>
              <code>SCRIPT_STR("/lowercase?unicode=&lt;unicode&gt;", str_value)</code>
            </div>
            <div class="params">
              <h4>Params</h4>
              <table class="items">
                <tr>
                  <th>Name</th>
                  <th>Type</th>
                  <th>Description</th>
                </tr>
                <tr>
                  <td class="item-name">
                    <code>unicode</code>
                  </td>
                  <td class="item-type">boolean</td>
                  <td class="item-desc">
                    <em>(Optional)</em>
                    Whether unicode logic should be used
                  </td>
                </tr>
              </table>
            </div>
            <div class="args">
              <h4>Arguments</h4>
              <table class="items">
                <tr>
                  <th>Name</th>
                  <th>Type</th>
                  <th>Description</th>
                </tr>
                <tr>
                  <td class="item-name">
                    <code>str_value</code>
                  </td>
                  <td class="item-type">string</td>
                  <td class="item-desc">Strings to be converted to lowercase</td>
                </tr>
              </table>
            </div>
            <div class="return">
              <h4>Return value</h4>
              <table class="items">
                <tr>
                  <th>Type</th>
                  <th>Description</th>
                </tr>
                <tr>
                  <td>character</td>
                  <td>A lowercase string</td>
                </tr>
              </table>
            </div>
          </div>
          <div class="route">
            <h3 id="/concat" class="path">
              /concat
              <a class="permalink" href="#/concat">#</a>
            </h3>
            <div class="desc">
              <h4>Description</h4>
              <div>Concatenate</div>
            </div>
            <div class="usage">
              <h4>Usage</h4>
              <code>SCRIPT_STR("/concat?sep=&lt;sep&gt;", arg1, arg2)</code>
            </div>
            <div class="params">
              <h4>Params</h4>
              <table class="items">
                <tr>
                  <th>Name</th>
                  <th>Type</th>
                  <th>Description</th>
                </tr>
                <tr>
                  <td class="item-name">
                    <code>sep</code>
                  </td>
                  <td class="item-type">string</td>
                  <td class="item-desc">
                    <em>(Optional)</em>
                    Separator value to use
                  </td>
                </tr>
              </table>
            </div>
            <div class="args">
              <h4>Arguments</h4>
              <table class="items">
                <tr>
                  <th>Name</th>
                  <th>Type</th>
                  <th>Description</th>
                </tr>
                <tr>
                  <td class="item-name">
                    <code>arg1</code>
                  </td>
                  <td class="item-type">string</td>
                  <td class="item-desc">One or more string values</td>
                </tr>
                <tr>
                  <td class="item-name">
                    <code>arg2</code>
                  </td>
                  <td class="item-type">string</td>
                  <td class="item-desc">One or more string values to concatenate to `arg1`</td>
                </tr>
              </table>
            </div>
            <div class="return">
              <h4>Return value</h4>
              <table class="items">
                <tr>
                  <th>Type</th>
                  <th>Description</th>
                </tr>
                <tr>
                  <td>character</td>
                  <td>arg1 and arg2 concatenated together</td>
                </tr>
              </table>
            </div>
          </div>
          <div class="route">
            <h3 id="/stringify" class="path">
              /stringify
              <a class="permalink" href="#/stringify">#</a>
            </h3>
            <div class="desc">
              <h4>Description</h4>
              <div>Convert to string</div>
            </div>
            <div class="usage">
              <h4>Usage</h4>
              <code>SCRIPT_STR("/stringify", value)</code>
            </div>
            <div class="args">
              <h4>Arguments</h4>
              <table class="items">
                <tr>
                  <th>Name</th>
                  <th>Type</th>
                  <th>Description</th>
                </tr>
                <tr>
                  <td class="item-name">
                    <code>value</code>
                  </td>
                  <td class="item-type">any</td>
                  <td class="item-desc">One or more values of any data type</td>
                </tr>
              </table>
            </div>
            <div class="return">
              <h4>Return value</h4>
              <table class="items">
                <tr>
                  <th>Type</th>
                  <th>Description</th>
                </tr>
                <tr>
                  <td>character</td>
                  <td>The data, converted to string</td>
                </tr>
              </table>
            </div>
          </div>
        </div>
      </main>
      
      </div>
      
      </body>
      </html>

