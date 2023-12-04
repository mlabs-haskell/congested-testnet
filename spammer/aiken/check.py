import binascii

def read_cbor_file(file_path):
    """Reads a file containing raw CBOR bytes and prints the content in hex format."""
    try:
        with open(file_path, 'rb') as file:  # Open the file in binary read mode
            raw_bytes = file.read()  # Read the entire file as bytes
            hex_data = binascii.hexlify(raw_bytes)  # Convert bytes to hex
            print(hex_data.decode('utf-8'))  # Print hex string
    except FileNotFoundError:
        print(f"File not found: {file_path}")
    except IOError as e:
        print(f"IO Error: {e}")

# Example usage
file_path = 'program.flat'
read_cbor_file(file_path)


def hex_to_cbor_file(hex_string, output_file_path):
    """Converts a hex string representing CBOR data to bytes and writes it to a file."""
    try:
        # Convert the hex string back to bytes
        cbor_bytes = binascii.unhexlify(hex_string)

        # Write the bytes to a file
        with open(output_file_path, 'wb') as file:
            file.write(cbor_bytes)

        print(f"Data written to {output_file_path}")

    except binascii.Error:
        print("Error: Input string is not valid hexadecimal.")
    except IOError as e:
        print(f"IO Error: {e}")

# Example usage
# hex_string = '510100003222253330044a229309b2b2b9a1'  # Replace with your CBOR hex string
# hex_string = '0100003222253330044a229309b2b2b9a1'  # Replace with your CBOR hex string
hex_string = '02000033222220051200120011'  # Replace with your CBOR hex string
output_file_path = 'temp.cbor'
hex_to_cbor_file(hex_string, output_file_path)

