import boto3
import os

# Get environment variables
aws_region = os.getenv('AWS_S3_REGION')
bucket_name = os.getenv('AWS_S3_BUCKET_NAME')
access_key = os.getenv('AWS_ACCESS_KEY_ID')
secret_key = os.getenv('AWS_SECRET_ACCESS_KEY')

# Create S3 client
s3_client = boto3.client(
    's3',
    endpoint_url='http://localhost:4566',
    aws_access_key_id=access_key,
    aws_secret_access_key=secret_key,
    region_name=aws_region
)

# Create bucket if it doesn't exist
try:
    s3_client.head_bucket(Bucket=bucket_name)
except:
    s3_client.create_bucket(
        Bucket=bucket_name,
        CreateBucketConfiguration={
            'LocationConstraint': aws_region
        } if aws_region != 'us-east-1' else {}
    )
    print(f"Created bucket: {bucket_name}")
